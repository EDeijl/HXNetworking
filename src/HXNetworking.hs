module HXNetworking where
import Network.Simple.TCP
import Graphics.UI.SDL as SDL
import System.Exit
import System.Random

connectToSocket :: IO ()
connectToSocket = connect "google.com" "80" $ \(connectionSocket, remoteAddr) -> do
                                                    putStrLn $ "Connection established to " ++ show remoteAddr

display :: Renderer -> IO ()
display renderer = do
        renderClear renderer
        r <- randomRIO (0,255)
        g <- randomRIO (0,255)
        b <- randomRIO (0,255)
        setRenderDrawColor renderer r g b 255
        renderPresent renderer

loop :: IO () -> IO ()
loop display
     = do event <- waitEvent
          putStrLn $ show event
          case fmap eventData event of
            Just Quit -> exitWith ExitSuccess
            Just (Keyboard KeyDown _ _ (Keysym Q _ _)) -> exitWith ExitSuccess
            Just (Keyboard KeyDown _ _ (Keysym SDL.Space _ _)) -> do putStrLn "Space event"
                                                                     connectToSocket
                                                                     display
            Just (MouseButton _ _ LeftButton Pressed _) -> do putStrLn "Mouse left event"
                                                              connectToSocket
                                                              display
            Just (TouchFinger TouchFingerDown _ _ _ _ _ _ _ _) -> do putStrLn "Touch event"
                                                                     connectToSocket
                                                                     display
            _ -> return ()
          loop display
