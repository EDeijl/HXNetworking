{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Events
import qualified Graphics.UI.SDL            as SDL
import           Reactive
import           Reactive.Banana
import           Reactive.Banana.Frameworks

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480
main :: IO ()
main = do
  SDL.init [SDL.InitVideo]
  window <- SDL.createWindow "HXNetworking" (SDL.Position 0 0) (SDL.Size windowWidth windowHeight) []
  glContext <- SDL.glCreateContext window
  renderer <- SDL.createRenderer window (SDL.Device (-1)) []
  (frameAddHandler, fireFrame) <- newAddHandler
  (eventAddHandler, fireEvent) <- newAddHandler
  network <- compile (makeNetwork window renderer glContext frameAddHandler eventAddHandler)
  actuate network

  forkIO . forever $ do
    fireFrame ()
    threadDelay 1000000
  eventLoop fireEvent
