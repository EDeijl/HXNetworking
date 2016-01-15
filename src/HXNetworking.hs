{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HXNetworking where
import           Control.Monad              hiding (mapM_)
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid

import           Data.Char
import           Data.Function
import           Data.Word
import           Foreign.C.Types
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Image      as Image
import           Graphics.UI.SDL.Surface
import           Paths_HXNetworking
import           Prelude                    hiding (any, mapM_)
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           HXNetworking.SDL
import           System.Random
import           HXNetworking.Types
import           HXNetworking.Utils

-- | State of the application
data AppState = AppState { asScore  :: Int
                         , asRand   :: StdGen
                         , fRect :: FillRect
                         }

-- | Type synonym for a list of Rects
type Grid = [SDL.Rect]

data FillRect = FillRect { rect  :: Rect
                         , color :: Color }


screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480

moduleMain:: IO ()
moduleMain = do
  -- SDL event source
  sdlES <- getSDLEventSource
  -- call initialization
  gd <- liftIO initGraphics
  network <- compile $ makeNetwork sdlES gd
  actuate network
  runSDLPump gd sdlES

-- | SDL initialization
initGraphics :: IO GraphicsData
initGraphics = do
  SDL.init [SDL.InitVideo]
  Image.init [initPng]
  window <- SDL.createWindow "HXNetworking" (SDL.Position 0 0 )(SDL.Size screenWidth screenHeight) []
  renderer <- SDL.createRenderer window (SDL.Device (-1)) [SDL.Software]
  return $ GraphicsData window renderer


-- | Render the rectangle
render ::GraphicsData -> AppState -> IO ()
render gd as= do
  setRenderDrawColor (renderer gd) r g b a
  renderDrawRect (renderer gd) (rect $ fRect as)
  renderFillRect (renderer gd) (rect $ fRect as)
  renderPresent (renderer gd)
  where Color r g b a = color $ fRect as

-- | setup the FRP network
makeNetwork :: forall t. Frameworks t => SDLEventSource -> GraphicsData -> Moment t ()
makeNetwork es gd= do
  r<- liftIO getStdGen
  eTickDiff <- tickDiffEvent es
  esdl <- sdlEvent es
  let
    -- | the initial state
    asInitial :: AppState
    asInitial = AppState 0 r (FillRect (Rect 100 100 100 100) (Color 255 0 0 255))
    -- | app state update event
    eASChange = (updateAS <$> eTickDiff) `union` (updateASOnKey <$> keyDownEvent esdl) `union` (updateASOnMouseOver <$> mouseButtonDownEvent esdl)
    -- | app state behavior
    bAppState = accumB asInitial eASChange
  -- | appState change event
  eApp <- changes bAppState
  reactimate' $ fmap (render gd) <$> eApp



-- | update app state on key press
updateASOnKey :: SDL.Keysym -> AppState -> AppState
updateASOnKey k as@(AppState s asRand fRect) =  AppState s r''' (FillRect f' (Color r g b 255))
  where (r, r')   = randomR (0, 255) asRand
        (g, r'')  = randomR (0, 255) r'
        (b, r''') = randomR (0, 255) r''
        f' = rect fRect

-- | update app state on tick
updateAS :: Word32 -> AppState -> AppState
updateAS _ as= as

-- | update appstate on mouseButtonDown events
updateASOnMouseButtonDown :: MouseButton -> AppState -> AppState
updateASOnMouseButtonDown _ as = AppState s r''' (FillRect f' (Color r g b 255))
  where (r, r')   = randomR (0, 255) asRand
        (g, r'')  = randomR (0, 255) r'
        (b, r''') = randomR (0, 255) r''
        f' = rect fRect

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = do
  surface <- load path
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  return texture

renderTexture :: Texture -> Renderer -> Position -> Maybe Rect -> IO ()
renderTexture texture renderer (Position x y) mbClip = do
  dst <- case mbClip of
    Nothing -> do
      Size w h <- queryTexture texture
      return $ Rect (x - (w `div` 2)) (y - (h `div` 2)) w h
    Just (Rect _x _y w h) -> return $ Rect (x - (w `div` 2)) (y - (h `div` 2)) w h
  print dst
  renderCopy renderer texture mbClip (Just dst)
