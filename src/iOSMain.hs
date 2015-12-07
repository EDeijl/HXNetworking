{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad              hiding (mapM_)
import           CUtil
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Events

import           Foreign.C.Types
import           Graphics.UI.SDL            as SDL
import           Prelude                    hiding (any, mapM_)
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

render :: Window -> Renderer -> Rect -> IO ()
render window renderer rect = do
  print window
  setRenderDrawColor renderer 255 255 255 255
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
