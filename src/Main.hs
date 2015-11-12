{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent
import           Control.Monad
import           Graphics.UI.SDL    as SDL
import           System.Random
import           System.Exit

screenWidth = 380
screenHeight = 480

render :: Renderer -> IO ()
render renderer = do
       putStrLn "render frame"
       setRenderDrawColor renderer 0 0 0 255
       renderClear renderer
       w <- randomRIO (64, 128)
       h <- randomRIO (64, 128)
       x <- randomRIO (0 , screenWidth)
       y <- randomRIO (0 , screenHeight)
       let rect = Rect x y w h
       r <- randomRIO (50, 255)
       g <- randomRIO (50, 255)
       b <- randomRIO (50, 255)
       setRenderDrawColor renderer r g b 255
       renderFillRect renderer rect
       renderPresent renderer

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = withInit [InitVideo] $ do
     window <- createWindow "test" (Position 0 0) (Size screenWidth screenHeight) [WindowShown]
     renderer <- createRenderer window (Device (-1)) []
     let loop = do
                 render renderer
                 event <- pollEvent
                 print event
                 case fmap eventData event of
                   Just Quit -> exitSuccess
                   _ -> return ()
                 loop
     loop
