module Types where
import qualified Graphics.UI.SDL as SDL
-- | gameplay input events
data InputEvent = UpPressed | DownPressed
     deriving Show

-- | the object to draw
type Paddle = SDL.Rect
