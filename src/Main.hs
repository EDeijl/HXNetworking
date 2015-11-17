{-# LANGUAGE OverloadedStrings #-}
import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.SDL           as SDL
import           System.Exit

screenWidth, screenHeight :: Int
screenWidth = 380
screenHeight = 480

render :: SDL.Window -> SDL.GLContext -> IO ()
render window context = do
       putStrLn "render frame"
       clearColor $= Color4 0 0 0 1
       clear [ColorBuffer]
       SDL.glSwapWindow window

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = SDL.withInit [SDL.InitVideo] $ do
     window <- SDL.createWindow "test" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) [SDL.WindowShown]
     glContext <- SDL.glCreateContext window
     let loop = do
                 render window glContext
                 event <- SDL.pollEvent
                 print event
                 case fmap SDL.eventData event of
                   Just SDL.Quit -> exitSuccess
                   _ -> return ()
                 loop
     loop
