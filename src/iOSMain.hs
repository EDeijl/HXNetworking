{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Events
import           Foreign.C.Types
import           Graphics.UI.SDL            as SDL
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.Random
import           Types

screenWidth, screenHeight :: Int
screenWidth = 640
screenHeight = 480
foreign export ccall "haskell_main" main :: IO ()
main = do
  SDL.init [SDL.InitVideo]
  window <- SDL.createWindow "HXNetworking" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) [WindowFullscreenDesktop]
  renderer <- SDL.createRenderer window (SDL.Device (-1)) []
  g <- newStdGen
  (frameAddHandler, fireFrame) <- newAddHandler
  (eventAddHandler, fireEvent) <- newAddHandler
  network <- compile (makeNetwork window renderer g frameAddHandler eventAddHandler)
  actuate network

  let loop = do
           fireFrame ()
           e <- SDL.waitEvent
           case fmap SDL.eventData e of
             Just Quit -> SDL.quit
             Just ev -> print ev >> fireEvent ev >> loop
             _ -> loop
  loop

rectAtPosition :: (Int, Int) -> Rect -> Rect
rectAtPosition (x, y) rect = Rect x  y (rectW rect) (rectH rect)
randomRect :: StdGen -> Rect
randomRect gen =
  Rect x y w h
  where w = fst $ randomR (64, 128) gen
        h = fst $ randomR (64, 128) gen
        x = fst $ randomR (0, screenWidth) gen
        y = fst $ randomR (0, screenHeight) gen

initRect :: Rect
initRect = Rect 100 100 100 100

render :: Window -> Renderer -> Rect -> IO ()
render window renderer rect = do
  print window
  print rect
  setRenderDrawColor renderer 0 0 0 255
  renderClear renderer
  setRenderDrawColor renderer 255 0 0 255
  renderFillRect renderer rect
  renderPresent renderer

makeNetwork :: forall t. Frameworks t => SDL.Window -> SDL.Renderer -> StdGen -> AddHandler () -> AddHandler EventData -> Moment t ()
makeNetwork window renderer g frameAddHandler eventAddHandler = do
  frames <- fromAddHandler frameAddHandler
  events <- fromAddHandler eventAddHandler
  let bRect :: Behavior t Rect
      bRect = accumB initRect (handleSDLEvent <$> events)
  eRect <- changes bRect
  reactimate' $ fmap (render window renderer ) <$> eRect

handleSDLEvent :: EventData -> Rect -> Rect
handleSDLEvent event rect = case event of
                              MouseMotion _ _ _ pos _ _ -> rectAtPosition (positionX pos, positionY pos) rect
                              --TouchFinger _ _ _ _ x y _ _ _ -> rectAtPosition (round (x * 1000 / 65535 * fromIntegral screenWidth) , round (y * 1000 / 65535 * fromIntegral screenHeight) ) rect
                              _ -> rect



inputCoordsToScreenCoords :: (CFloat, CFloat) -> (Int, Int)
inputCoordsToScreenCoords (x, y) = (round (fromIntegral screenWidth * x), round (fromIntegral screenHeight * y))

screenCoordsToInputCoords :: (Int, Int) -> (Float, Float)
screenCoordsToInputCoords (x, y) = (fromIntegral x/ fromIntegral screenWidth , fromIntegral y/ fromIntegral screenWidth)
