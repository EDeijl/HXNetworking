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
import           HXNetworking.SDL
import           HXNetworking.Types
import           HXNetworking.Utils
import           Paths_HXNetworking
import           Prelude                    hiding (any, mapM_)
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Random

-- | State of the application
data AppState = AppState { asScore :: Int
                         , asRand  :: StdGen
                         , fRect   :: FillRect
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
  w <- SDL.createWindow "HXNetworking" (SDL.Position 0 0 )(SDL.Size screenWidth screenHeight) []
  r <- SDL.createRenderer w (SDL.Device (-1)) [SDL.Software]
  return $ GraphicsData w r

-- | Render Rectangle
renderRect :: GraphicsData -> FillRect -> IO ()
renderRect gd fRect = do
  setRenderDrawColor (renderer gd) r g b a
  renderDrawRect (renderer gd) (rect fRect)
  renderFillRect (renderer gd) (rect fRect)
  --renderPresent (renderer gd)
  where Color r g b a = color fRect


-- | Render the rectangle
render ::GraphicsData -> AppState ->  IO ()
render gd _ = renderPresent (renderer gd)

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

    initialButton1 ::  FillRect
    initialButton1 = FillRect (Rect 100 200 100 100) (Color 255 0 0 255)

    initialButton2 ::  FillRect
    initialButton2 = FillRect (Rect 400 200 100 100) (Color 255 0 0 255)

    eButton button = mouseButtonDownEvent (mouseEventWithin (rect button) esdl)


    eButton1MouseDown = updateRectOnMouseDown <$> mouseButtonDownEvent (mouseEventWithin (rect initialButton1) esdl)
    eButton1MouseUp = updateRectOnMouseUp <$> mouseButtonUpEvent (mouseEventWithin (rect initialButton1) esdl)

    eButton2MouseDown = updateRectOnMouseDown <$> mouseButtonDownEvent (mouseEventWithin (rect initialButton2) esdl)
    eButton2MouseUp = updateRectOnMouseUp <$> mouseButtonUpEvent (mouseEventWithin (rect initialButton2) esdl)

    eButton1 = eButton1MouseDown `union` eButton1MouseUp `union` (updateRect <$> eTickDiff)
    eButton2 = eButton2MouseDown `union` eButton2MouseUp `union` (updateRect <$> eTickDiff)

    bButton1 = accumB initialButton1 eButton1
    bButton2 = accumB initialButton2 eButton2

    -- | app state update event
    eASChangeKeys = (updateAS <$> eTickDiff) `union` (updateASOnKey <$> keyDownEvent esdl)
    eASChangeMouse = updateASOnMouseButtonDown <$> mouseButtonDownEvent (mouseEventWithin (rect $ fRect asInitial) esdl)
    -- eASMouseMovement = updateASOnMovement <$> mouseMovementEvent esdl
    eASChange = eASChangeKeys `union` eASChangeMouse
    checkWithin rect = mouseEventWithin rect esdl
    -- | app state behavior
    bAppState = accumB asInitial eASChange
  -- | appState change event
  eApp <- changes bAppState
  eButton1Changes <- changes bButton1
  eButton2Changes <- changes bButton2
  reactimate' $ fmap (renderRect gd) <$> eButton1Changes
  reactimate' $ fmap (renderRect gd) <$> eButton2Changes
  reactimate' $ fmap (render gd) <$> eApp

-- | update app state on mouse movement
updateASOnMovement :: SDL.EventData -> AppState -> AppState
updateASOnMovement (MouseMotion _ _ _ pos _ _) (AppState s asRand fRect ) = AppState s asRand (FillRect f' (color fRect))
  where f' = Rect (positionX pos - (rectW (rect fRect) `div` 2))
                  (positionY pos - (rectH (rect fRect) `div` 2))
                  (rectW (rect fRect))
                  (rectH (rect fRect))

updateRectOnMouseDown ::  EventData -> FillRect -> FillRect
updateRectOnMouseDown _ fRect= FillRect f' (color fRect)
  where f' = Rect (rectX (rect fRect) - (rectW (rect fRect)) `div` 2)
                  (rectY (rect fRect) - (rectH (rect fRect)) `div` 2)
                  w h
        w = (2 * rectW (rect fRect))
        h = (2 * rectH (rect fRect))

updateRectOnMouseUp ::  EventData -> FillRect -> FillRect
updateRectOnMouseUp _ fRect= FillRect f' (color fRect)
  where f' = Rect (rectX (rect fRect) + w `div` 2)
                  (rectY (rect fRect) + h `div` 2) w h
        w = (rectW (rect fRect) `div` 2)
        h = (rectH (rect fRect) `div` 2)

-- | update app state on key press
updateASOnKey :: SDL.Keysym -> AppState -> AppState
updateASOnKey k as@(AppState s asRand fRect ) = AppState s r''' (FillRect f' (Color r g b 255))
  where (r, r')   = randomR (0, 255) asRand
        (g, r'')  = randomR (0, 255) r'
        (b, r''') = randomR (0, 255) r''
        f' = rect fRect

-- | update app state on tick
updateAS :: Word32 -> AppState -> AppState
updateAS _ as= as

updateRect :: Word32 -> FillRect -> FillRect
updateRect _ rect = rect

-- | update appstate on mouseButtonDown events
updateASOnMouseButtonDown :: EventData -> AppState -> AppState
updateASOnMouseButtonDown (MouseButton _ _ button state position) as@(AppState s asRand fRect ) = AppState s r''' (FillRect f' (Color r g b 255))
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
