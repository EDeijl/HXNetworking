module HXNetworking where
import Graphics.UI.SDL as SDL
import System.Random

render :: SDL.Window -> SDL.Renderer -> SDL.GLContext -> IO ()
render window renderer context = do
        SDL.glSwapWindow window
        print window
        r <- randomRIO (0,255)
        g <- randomRIO (0,255)
        b <- randomRIO 
               (0,255)
        SDL.setRenderDrawColor renderer r g b 255
        SDL.renderClear renderer
        SDL.renderPresent renderer
