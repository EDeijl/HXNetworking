module SDL where

import           Control.Monad
import           Data.Word
import           Graphics.UI.SDL            as SDL
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (newAddHandler)
import           Types
import           Utils


getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

-- | one step in the main event loop, returning False when it needs to stop
mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump es = do
  let esdl = getSDLEvent es
      etick = getTickEvent es
  tick <- SDL.getTicks
  me <- collectEvents
  case me of
    Nothing -> return False
    Just e -> do
      fire esdl e
      fire etick tick
      return True

-- | collect SDL events
-- return Nothing on quit, otherwise a list, possibly empty, of events
collectEvents :: IO (Maybe [SDL.Event])
collectEvents = do
  e <- SDL.pollEvent
  case e of
    Just (Event _ Quit) -> return Nothing
    Nothing -> return (Just [])
    Just ev@_ -> liftM (liftM (ev:)) collectEvents

-- | main event loop
runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

-- | main event loop, capped at n frames / second
runCappedSDLPump :: Int -> SDLEventSource -> IO ()
runCappedSDLPump rate es = do
  startTick <- SDL.getTicks
  c <- mainSDLPump es
  endTick <- SDL.getTicks
  let ticks = fromIntegral (endTick - startTick)
      secsPerFrame = fromIntegral (1000 `div` rate)
  when (ticks < secsPerFrame) $
    delay $ secsPerFrame - ticks
  when c $ runCappedSDLPump rate es

