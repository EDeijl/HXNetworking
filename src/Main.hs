module Main where
import HXNetworking
import Graphics.UI.SDL

main :: IO ()
main = withInit [InitVideo] $ do
     window <- createWindow "HXSDL" (Position 0 0) (Size 640 480) [WindowShown]
     renderer <- createRenderer window (Device (-1)) [Accelerated, PresentVSync]
     loop (display renderer)
--connectToSocket "google.com" "80"
