{-# LANGUAGE OverloadedStrings #-}
module Main where
import HXNetworking
import Foreign.C
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mouse 

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = withInit [InitVideo, InitEvents] $ do
     window <- createWindow "HXSDL" (Position 0 0) (Size 640 480) [WindowOpengl]
     glSwapWindow window
     putStrLn "Created Window"
     renderer <- createRenderer window (Device (-1)) [Accelerated, PresentVSync]
     putStrLn "Created Renderer"
     loop (display renderer)
