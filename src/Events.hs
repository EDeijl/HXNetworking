module Events where
import           Data.Function   (fix)
import qualified Graphics.UI.SDL as SDL
import           Types

eventLoop :: (InputEvent -> IO a) -> IO ()
eventLoop fireEvent =
  fix $ \loop -> do
      e <- SDL.waitEvent
      case fmap SDL.eventData e of
        Just (SDL.Keyboard SDL.KeyDown _ _ (SDL.Keysym keycode _ _)) -> case keycode of
                                                                          SDL.Up   -> fireEvent UpPressed   >> loop
                                                                          SDL.Down -> fireEvent DownPressed >> loop
                                                                          SDL.Q    -> SDL.quit
                                                                          _ -> loop
        Just (SDL.TouchFinger SDL.TouchFingerMotion _ _ _ _ _ _ dy _) -> if dy > 0
                                                                         then fireEvent DownPressed >> loop
                                                                         else fireEvent UpPressed >> loop
        _ -> loop

