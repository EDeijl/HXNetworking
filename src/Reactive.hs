{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive where
import qualified Graphics.UI.SDL            as SDL
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Types

-- create the FRP network
makeNetwork :: forall t. Frameworks t => SDL.Window -> SDL.Renderer -> AddHandler () -> AddHandler InputEvent -> Moment t ()
makeNetwork window renderer frameAddHandler eventAddHandler = do
  frames <- fromAddHandler frameAddHandler
  events <- fromAddHandler eventAddHandler

  let bpaddle :: Behavior t Paddle
      bpaddle = accumB initPaddle (f <$> events)
        where
          f :: InputEvent -> Paddle -> Paddle
          f UpPressed = paddleUp
          f DownPressed = paddleDown
  epaddle <- changes bpaddle
  reactimate' $ fmap (renderPaddle window renderer ) <$> epaddle

-- | create an initial paddle at location and with size
initPaddle :: Paddle
initPaddle  = SDL.Rect 100 100 20 20

-- | what needs to happen to make the paddle move down
paddleDown :: Paddle -> Paddle
paddleDown (SDL.Rect x y w h) = SDL.Rect x (y + 30) w h

-- | what needs to happen to make the paddle move up
paddleUp :: Paddle -> Paddle
paddleUp (SDL.Rect x y w h) = SDL.Rect x (y - 30) w h

-- | render the paddle
renderPaddle :: SDL.Window -> SDL.Renderer -> Paddle -> IO ()
renderPaddle window renderer p = do
  print window
  SDL.setRenderDrawColor renderer 255 255 255 255
  SDL.renderClear renderer
  SDL.setRenderDrawColor renderer 0 0 255 255
  SDL.renderFillRect renderer p
  SDL.renderPresent renderer
