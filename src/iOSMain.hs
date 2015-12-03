{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Events
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
  window <- SDL.createWindow "HXNetworking" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) []
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
             _ -> fireEvent e >> loop
  loop

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
  setRenderDrawColor renderer 0 0 0 255
  renderClear renderer
  r <- randomRIO (50, 255)
  g <- randomRIO (50, 255)
  b <- randomRIO (50, 255)
  setRenderDrawColor renderer r g b 255
  renderFillRect renderer rect
  renderPresent renderer

makeNetwork :: forall t. Frameworks t => SDL.Window -> SDL.Renderer -> StdGen -> AddHandler () -> AddHandler (Maybe SDL.Event) -> Moment t ()
makeNetwork window renderer g frameAddHandler eventAddHandler = do
  frames <- fromAddHandler frameAddHandler
  events <- fromAddHandler eventAddHandler
  let bRect :: Behavior t Rect
      bRect = accumB initRect (f <$> events)
        where
          f :: (Maybe SDL.Event) -> Rect -> Rect
          f _ _ = randomRect g
  eRect <- changes bRect
  reactimate' $ fmap (render window renderer ) <$> eRect
