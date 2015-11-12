module HXNetworking where
import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.SDL       as SDL
import           Graphics.UI.SDL.Mouse
import           Network.Simple.TCP
import           System.Exit
import           System.Random

connectToSocket :: IO ()
connectToSocket = connect "google.com" "80" $ \(connectionSocket, remoteAddr) ->
                           putStrLn $ "Connection established to " ++ show remoteAddr

display :: Renderer -> IO ()
display renderer = do
        putStrLn "render tick"
        setRenderDrawColor renderer 0 0 0 255
        renderClear renderer
        -- come up with a random rectangle
        x <- randomRIO (0,320)
        y <- randomRIO (0,480)
        w <- randomRIO (64, 128)
        h <- randomRIO (64, 128)
        r <- randomRIO (50,255)
        g <- randomRIO (50,255)
        b <- randomRIO (50,255)
        setRenderDrawColor renderer r g b 255
        renderFillRect renderer (Rect x y w h)
        renderPresent renderer

loop :: IO () -> IO ()
loop display = do
   display
   --event <- pollEvent
   --case fmap eventData event of
         --Just Quit -> exitSuccess
         --Just (Keyboard KeyDown _ _ (Keysym Q _ _)) -> exitSuccess
         --Just (Keyboard KeyDown _ _ (Keysym SDL.Space _ _)) -> putStrLn "Space event"
         --Just (MouseButton _ _ LeftButton Pressed _) -> putStrLn "Mouse left event"
         --Just (TouchFinger TouchFingerDown _ _ _ _ _ _ _ _) -> putStrLn "Touch event"
         --Just _ -> print event
         --_ -> return ()
   threadDelay (10^6 *2)
   loop display

waitForForegroundEvent :: IO ()
waitForForegroundEvent = do
  putStrLn "Waiting for AppDidEnterForeground"
  mbEv <- waitEvent
  let handleEvent e = case eventData e of
                        AppDidEnterForeground -> return ()
                        _ -> waitForForegroundEvent
  maybe waitForForegroundEvent handleEvent mbEv
