module Reactive.Banana.SDL.Types (EventSource, SDLEventSource (..), WrappedEvent, TickEvent) where

import           Data.Word
import           Graphics.UI.SDL            as SDL
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (AddHandler)

-- | Generic Event Source
type EventSource a = (AddHandler a, a -> IO ())
-- | an event containing a list of SDL event
type WrappedEvent t = R.Event t [SDL.Event]
-- | SDL Tick event
type TickEvent t = R.Event t Word32
-- | SDL Event Source
data SDLEventSource = SDLEventSource { getSDLEvent  :: EventSource [SDL.Event]
                                     , getTickEvent :: EventSource Word32 }
