{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified Graphics.UI.SDL              as SDL
import qualified Graphics.UI.SDL.TTF          as TTF
import qualified Graphics.UI.SDL.TTF.Types    as TTF

import           Reactive.Banana
import           Reactive.Banana.SDL
import           Reactive.Banana.SDL.Graphics

import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Word
import           System.FilePath
import           System.Random

import           Control.Monad.IO.Class       (liftIO)
import           Paths_HXNetworking
import           Reactive.Banana.Frameworks   (Frameworks, actuate)

screenWidth, screenHeight :: Int
screenWidth = 380
screenHeight = 480

main :: IO ()
main = do
     sdlES <- getSDLEventSource
     putStrLn "get sdl event source"
     graphicsInit <- liftIO initGraphics
     putStrLn "initialized all graphics"
     network <- compile $ setupNetwork sdlES graphicsInit
     putStrLn "Compiled the FRP network"
     actuate network
     putStrLn "actuate the FRP network"
     runCappedSDLPump 30 sdlES

initGraphics :: IO GraphicsData
initGraphics =
     SDL.withInit [SDL.InitVideo, SDL.InitEvents] $
     TTF.withInit $ do
     dd <-getDataDir
     realFont <-TTF.openFont (dd </> "font" </> "FreeSansBold.ttf") 24
     window <- SDL.createWindow "test" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) [SDL.WindowShown]
     putStrLn "Created Window"
     print window
     err <- SDL.getError
     mapM_ putStrLn $ fmap ("error in initGraphics: " ++) err
     maybeSurface <- SDL.getWindowSurface window
     print maybeSurface
     err <- SDL.getError
     mapM_ putStrLn $ fmap ("error in initGraphics: " ++) err
     return $ GraphicsData realFont maybeSurface window

setupNetwork :: Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
setupNetwork es gd = do
  r <- liftIO getStdGen
  eTickDiff <- tickDiffEvent es
  esdl <- sdlEvent es
  let
      gsInitial :: GameState
      gsInitial = GameState 0 100 2000 M.empty r 0 0 lives
      startGraphic :: Graphic
      --startGraphic = draw (Fill (SDL.Rect 0 0 width height) black) (Mask Nothing 0 0)
      startGraphic = draw (Image "image.bmp") (Mask Nothing 0 0)
      bScreen :: Behavior t Screen
      bScreen = pure $ fromJust (gd_mainSurface gd)
      eGSChange = (updateGS <$> eTickDiff) `union` (updateGSOnKey <$> keyDownEvent esdl)
      bGameState =accumB gsInitial eGSChange
      --livesG GameState{gs_lives}=draw (Text ("Lives:" ++ show gs_lives ++ "/" ++ show lives) (gd_font gd) red) (Mask Nothing 0 0)
      ---- | draw score
      --scoreG GameState{gs_score}=draw (Text ("Score:" ++ show gs_score) (gd_font gd) red) (Mask Nothing (halfW+1) 0)
      ---- | draw a character
      --charG (c,(x,y))= draw (Text [c] (gd_font gd) white) (Mask Nothing x y)
      ---- | draw characters
      --charsG GameState{gs_shown}=let
                --chars=map charG (M.assocs gs_shown)
                --in (Graphic $ \surface ->mapM_ (\(Graphic f)->void $ f surface) chars >> return Nothing)
      ---- | game over
      --gameOverG GameState{gs_score}=draw (Text "Game Over!" (gd_font gd) red) (Mask Nothing (halfW-40) (halfH-20))
                                      --`over`
                                      --draw (Text ("Score:" ++ show gs_score) (gd_font gd) red) (Mask Nothing (halfW-40) (halfH+10))
      bG= (\g-> startGraphic) <$> bGameState
  renderGraph bG bScreen (gd_window gd)
  return ()

updateGS :: Word32 -> GameState -> GameState
updateGS d gs1 = let
         mvs = gs_moves gs1 + fromIntegral d
         gsInc = changeif shouldSpeed speedup gs1{gs_moves=mvs}
         gsMoved = changeif ((0 ==) . mod mvs . gs_movespeed) moveDown gsInc
         alive = gs_lives gsMoved > 0
         gsNew = if alive
                    then changeif ((0==). mod mvs . gs_newspeed ) newChar gsMoved
                    else gsMoved
         in gsNew

-- | start number of lives
lives :: Int
lives = 3

-- | red color
red :: SDL.Color
red = SDL.Color 255 20 20 255

-- | black color
black :: SDL.Color
black = SDL.Color 0 0 0 255

-- | white color
white :: SDL.Color
white=SDL.Color 255 255 255 255

width, height :: Int
width = 640
height = 480

halfW, halfH :: Int
halfW = div width 2
halfH = div height 2

data GraphicsData = GraphicsData {
 gd_font        :: TTF.TTFFont,
 gd_mainSurface :: Maybe SDL.Surface,
 gd_window      :: SDL.Window
}

data GameState = GameState {
     gs_moves             :: Int
   , gs_movespeed         :: Int
   , gs_newspeed          :: Int
   , gs_shown             :: M.Map Char (Int, Int)
   , gs_rand              :: StdGen
   , gs_score             :: Int
   , gs_score_beforespeed :: Int
   , gs_lives             :: Int
}

updateGSOnKey :: SDL.Keysym -> GameState -> GameState
updateGSOnKey ks gs@GameState{gs_shown, gs_score}=
              let c= SDL.keyScancode ks
              in case c of
                      SDL.KeypadPlus -> speedup gs
                      _ -> if M.member (head $ show c) gs_shown
                              then gs {gs_shown=M.delete (head $ show c) gs_shown, gs_score=gs_score+1}
                              else gs

changeif :: (a -> Bool) -> (a->a) -> a -> a
changeif test change obj = if test obj then change obj else obj

shouldSpeed :: GameState -> Bool
shouldSpeed GameState{gs_score,gs_score_beforespeed}= (gs_score-gs_score_beforespeed)>5

speedup :: GameState -> GameState
speedup gs@GameState{gs_score,gs_movespeed,gs_newspeed} = let
        ratio = 0.9::Double
        in gs{gs_movespeed=round (fromIntegral gs_movespeed * ratio), gs_newspeed=round (fromIntegral gs_newspeed * ratio), gs_score_beforespeed=gs_score}

newChar :: GameState -> GameState
newChar gs@GameState {gs_rand,gs_shown}=let
        (c,r')=randomR ('A', 'Z') gs_rand
        (s',r'')=if not (M.member c gs_shown)
                    then let (x,r2) = randomR (1,div width 10 -2) r'
                         in (M.insert c (x*10,20) gs_shown, r2)
                    else (gs_shown, r')
        in gs {gs_rand=r'',gs_shown=s'}

moveDown :: GameState -> GameState
moveDown gs@GameState{gs_shown,gs_lives}=let
        (dead,s')=M.foldWithKey  (\c (x,y) (b,m)->
                let y'=y+1
                in if y' > (height - 20) then (True, m) else (b, M.insert c (x, y') m)
                ) (False,M.empty) gs_shown
        d'=if dead then gs_lives-1 else gs_lives
        in gs{gs_shown=s',gs_lives=d'}
