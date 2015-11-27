-- | main SDL event loop
module Reactive.Banana.SDL ( module Reactive.Banana.SDL.Types
                           , module Reactive.Banana.SDL.Util
                           , getSDLEventSource, runSDLPump
                           , runCappedSDLPump ) where

import           Control.Monad
import           Data.Maybe
-- import           Data.Word
import           Graphics.UI.SDL            as SDL
-- import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (newAddHandler)
import           Reactive.Banana.SDL.Types
import           Reactive.Banana.SDL.Util

getSDLEventSource :: IO SDLEventSource
getSDLEventSource = SDLEventSource <$> newAddHandler <*> newAddHandler

-- | one step in the main event loop, returning False when it needs to stop
mainSDLPump :: SDLEventSource -> IO Bool
mainSDLPump es = do
    putStrLn "start main SDL pump"
    let esdl = getSDLEvent es
        etick = getTickEvent es
    tick <- SDL.getTicks
    putStrLn "Got ticks"
    me <- collectEvents
    putStrLn "collected events"
    case me of
      Nothing -> return False
      Just e -> do
           putStrLn $ "event: " ++ show e
           fire esdl e
           fire etick tick
           return True

-- | collect SDL events
-- return Nothing on quit, otherwise a list, possibly empty, of events
collectEvents :: IO (Maybe [SDL.Event])
collectEvents = do
    e <- pollEvent
    err <- SDL.getError
    mapM_ putStrLn $ fmap ("error in collectEvents: " ++) err
    case e of
        Just ev@(Event _ Quit ) -> putStrLn ("Event: " ++ show ev)  >> return Nothing
        Just ev@(Event _ _ ) -> putStrLn ("Event: " ++ show ev)  >> liftM (liftM ((fromJust e):)) collectEvents
        _ -> putStrLn "No event" >> return (Just [])

-- | main event loop
runSDLPump :: SDLEventSource -> IO ()
runSDLPump es = whileM (mainSDLPump es)

-- | main event loop, capped at n frames/second
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
