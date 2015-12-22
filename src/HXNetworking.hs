{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HXNetworking where
import           Control.Monad              hiding (mapM_)
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid

import           Data.Function
import           Foreign.C.Types
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Image      as Image
import           Paths_HXNetworking
import qualified Physics.Hipmunk            as H
import           Prelude                    hiding (any, mapM_)
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Random

-- | drawable object with a physics object attached to it.
data Object = Object { texture       :: Texture
                     , physicsCircle :: H.Body
                     , position      :: Position }

screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480

moduleMain:: IO ()
moduleMain = do
  SDL.init [SDL.InitVideo]
  Image.init [initPng]
  H.initChipmunk
  window <- SDL.createWindow "HXNetworking" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) []
  renderer <- SDL.createRenderer window (SDL.Device (-1)) []
  g <- newStdGen
  (frameAddHandler, fireFrame) <- newAddHandler
  (eventAddHandler, fireEvent) <- newAddHandler
  network <- compile (makeNetwork window renderer g frameAddHandler eventAddHandler)
  actuate network

  let loop = do
           let collectEvents = do
                 e <- SDL.pollEvent
                 case e of
                   Nothing -> return []
                   Just e' -> (e' :) <$> collectEvents
           events <- map SDL.eventData <$> collectEvents
           let (Any quit,Last event) =  -- only act on the last event in this frame and ignore all TouchFinger events
                 foldMap (\case
                             Quit -> (Any True, mempty)
                             e -> case e of
                                    TouchFinger {} -> mempty
                                    _ -> (mempty, Last $ Just e)) events
           case event of
             Just ev -> print ev >> fireEvent ev
             _ -> return ()
           fireFrame ()
           renderPresent renderer
           unless quit loop
  loop

rectAtPosition :: (Int, Int) -> Rect -> Rect
rectAtPosition (x, y) rect = Rect (x - (fromIntegral (rectW rect) `div` 2)) (y - (fromIntegral (rectH rect) `div` 2)) (rectW rect) (rectH rect)

randomRect :: StdGen -> Rect
randomRect gen =
  Rect x y w h
  where w = fst $ randomR (64, 128) gen
        h = fst $ randomR (64, 128) gen
        x = fst $ randomR (0, screenWidth) gen
        y = fst $ randomR (0, screenHeight) gen

initRect :: Rect
initRect = Rect 100 100 100 100

initObj :: Texture -> H.Body -> Position-> Object
initObj = Object

render :: Window -> Renderer -> Rect -> IO ()
render window renderer rect = do
  setRenderDrawColor renderer 255 255 255 255
  renderClear renderer
  print window
  setRenderDrawColor renderer 255 0 0 255
  renderFillRect renderer rect

makeNetwork :: forall t. Frameworks t => SDL.Window -> SDL.Renderer -> StdGen -> AddHandler () -> AddHandler EventData -> Moment t ()
makeNetwork window renderer g frameAddHandler eventAddHandler = do
  frames <- fromAddHandler frameAddHandler
  events <- fromAddHandler eventAddHandler
  image <- liftIO $ HXNetworking.loadTexture "sprites.png" renderer
  body <- liftIO $ H.newBody 10 0
  let bImg :: Behavior t Object
      bImg = pure (initObj image body (Position 50 50))
  let bRect :: Behavior t Rect
      bRect = accumB initRect (handleSDLEvent <$> events)
  eRect <- changes bRect
  reactimate' $ fmap (render window renderer) <$> eRect
  reactimate' $ fmap (renderTexture image renderer (Position 50 50) Nothing)

handleSDLEvent :: EventData -> Rect -> Rect
handleSDLEvent event rect = case event of
                              MouseMotion _ _ _ pos _ _ -> if  touchWithinRect (positionX pos, positionY pos) rect
                                                           then rectAtPosition (positionX pos, positionY pos) rect
                                                           else rect
                              _ -> rect

touchWithinRect :: (Int, Int) -> Rect -> Bool
touchWithinRect (x, y) rect = x > rectX rect && x < (rectX rect + rectW rect) &&
                              y > rectY rect && y < (rectY rect + rectH rect)

inputCoordsToScreenCoords :: (CFloat, CFloat) -> (Int, Int)
inputCoordsToScreenCoords (x, y) = (round (fromIntegral screenWidth * x), round (fromIntegral screenHeight * y))

screenCoordsToInputCoords :: (Int, Int) -> (Float, Float)
screenCoordsToInputCoords (x, y) = (fromIntegral x/ fromIntegral screenWidth , fromIntegral y/ fromIntegral screenWidth)

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = createTextureFromSurface renderer =<< load path


renderTexture :: Texture -> Renderer -> Position -> Maybe Rect -> IO ()
renderTexture texture renderer (Position x y) mbClip = do
  dst <- case mbClip of
    Nothing -> do
      Size w h <- queryTexture texture
      return $ Rect x y w h
    Just (Rect _x _y w h) -> return $ Rect x y w h
  renderCopy renderer texture mbClip (Just dst)
