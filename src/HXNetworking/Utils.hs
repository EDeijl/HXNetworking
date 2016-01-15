module HXNetworking.Utils where
import           Control.Monad              (liftM, when)
import           Graphics.UI.SDL            as SDL
import           Reactive.Banana            as R
import           Reactive.Banana.Frameworks (AddHandler, Frameworks,
                                             fromAddHandler)
import           HXNetworking.Types

-- | run while the given computation returns True
whileM :: IO Bool -> IO ()
whileM f = f >>= (\x -> when x $ whileM f )

-- | get the AddHandler from a EventSource
addHandler :: EventSource a -> AddHandler a
addHandler = fst

-- | fire the event from an Event Source
fire :: EventSource a -> a -> IO ()
fire = snd

-- | SDL event
sdlEvent :: Frameworks t => SDLEventSource -> Moment t (WrappedEvent t)
sdlEvent = fromAddHandler . addHandler . getSDLEvent

-- | SDL tick
tickEvent :: Frameworks t => SDLEventSource -> Moment t (TickEvent t)
tickEvent = fromAddHandler . addHandler . getTickEvent

-- | event carrying the difference between the last two SDL ticks
tickDiffEvent :: Frameworks t =>SDLEventSource -> Moment t (TickEvent t)
tickDiffEvent =liftM (successive (\a b->if b>a then Just (b-a) else Nothing)) . tickEvent

-- | filter any key events
keyEvent :: WrappedEvent t -> WrappedEvent t
keyEvent = collect . filterE isKey . spill
    where
        isKey e = case e of
          Event _ (Keyboard KeyUp _ _ _) -> True
          Event _ (Keyboard KeyDown _ _ _) -> True
          _ -> False

-- | event carrying the key pressed down
keyDownEvent :: WrappedEvent t -> R.Event t SDL.Keysym
keyDownEvent= filterJust . (isDown <$>) . spill . keyEvent
        where isDown (Event _ (Keyboard KeyDown _ _ k)) = Just k
              isDown _ = Nothing

-- | event carrying the key pressed up
keyUpEvent :: WrappedEvent t -> R.Event t SDL.Keysym
keyUpEvent= filterJust . (isDown <$>) . spill . keyEvent
        where isDown (Event _ (Keyboard KeyUp _ _ k)) = Just k
              isDown _ = Nothing

-- | filter any mouse event (button or move)
mouseEvent :: WrappedEvent t -> WrappedEvent t
mouseEvent esdl = mouseMotion `union` mouseButtonEvent esdl
    where
        mouseMotion = collect . filterE isMotion $ spill esdl
        isMotion e = case e of
          Event _  MouseMotion{} -> True
          _ -> False

-- | event carrying the mouse event down
mouseButtonDownEvent :: WrappedEvent t -> R.Event t SDL.EventData
mouseButtonDownEvent = filterJust . (isDown <$>) . spill . mouseEvent
  where isDown (Event _ b@(MouseButton _ _ _ Pressed _)) = Just b
        isDown _ = Nothing

-- | event carrying the mouse event up
mouseButtonUpEvent :: WrappedEvent t -> R.Event t SDL.EventData
mouseButtonUpEvent = filterJust . (isDown <$>) . spill . mouseEvent
  where isDown (Event _ b@(MouseButton _ _ _ Released _)) = Just b
        isDown _ = Nothing

-- | mouse button event
mouseButtonEvent :: WrappedEvent t -> WrappedEvent t
mouseButtonEvent = collect . filterE isButton . spill
    where
        isButton e = case e of
          Event _ MouseButton {} -> True
          _ -> False

-- | mouse event occuring inside a given area
mouseEventWithin :: Rect -> WrappedEvent t -> WrappedEvent t
mouseEventWithin ~(Rect x y w h) = collect . filterE isWithin . spill
    where
        within mx' my' = let (mx, my) = (fromIntegral mx', fromIntegral my') in (mx >= x && mx <= x + w) && (my >= y && my <= y + h)
        isWithin e = case e of
          Event _ (MouseMotion _ _ _ (Position mx my) _ _) -> within mx my
          Event _ (MouseButton _ _ _ Pressed (Position mx my)) -> within mx my
          Event _ (MouseButton _ _ _ Released (Position mx my)) -> within mx my
          _ -> False

filterEq :: Eq a => R.Event t a -> R.Event t a
filterEq = filterJust . fst . mapAccum Nothing . fmap f
    where
        f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
        f y Nothing  = (Just y, Just y)

-- | filter an event on a particular key being held down
keyFilter :: SDL.Keysym -> SDL.Event -> Bool
keyFilter k (Event _ (Keyboard KeyDown _ _ k')) | k == k' = True
keyFilter _ _ = False

-- | filter an event on a particular key being released
keyUpFilter :: SDL.Keysym -> SDL.Event -> Bool
keyUpFilter k (Event _ (Keyboard KeyUp _ _ k')) | k == k' = True
keyUpFilter _ _ = False

-- | filter if the function on two successive 'a's return a Just value
successive :: (a -> a -> Maybe b) -> R.Event t a -> R.Event t b
successive f e = filterJust (b <@> e)
    where b = stepper (const Nothing) (f <$> e)

-- | fires when the given key is pressed (down + up)
keyPressed :: SDL.Keysym -> WrappedEvent t -> WrappedEvent t
keyPressed k = collect . successive (\p c -> if keyFilter k p && keyUpFilter k c then Just c else Nothing) . spill . keyEvent

-- | fires when the specific button if clicked (down and up)
buttonClick :: MouseButton -> WrappedEvent t -> WrappedEvent t
buttonClick b = collect . successive sameButton . spill . mouseButtonEvent
  where sameButton (Event _(MouseButton _ _ b1 Pressed _)) e@(Event _ (MouseButton _ _ b2 Released _)) | b1 == b && b2 == b = Just e
        sameButton _ _ = Nothing


