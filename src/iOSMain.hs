{-# LANGUAGE OverloadedStrings #-}
module IOSMain where
import qualified Graphics.UI.SDL           as SDL
import           HXNetworking
import           System.Exit

screenWidth, screenHeight :: Int
screenWidth = 380
screenHeight = 480

foreign export ccall "haskell_main" main :: IO ()
main :: IO ()
main = SDL.withInit [SDL.InitVideo] $ do
     window <- SDL.createWindow "test" (SDL.Position 0 0) (SDL.Size screenWidth screenHeight) [SDL.WindowShown]
     glContext <- SDL.glCreateContext window
     renderer <- SDL.createRenderer window (SDL.Device (-1)) []
     let loop = do
                 event <- SDL.waitEvent
                 case fmap SDL.eventData event of
                   Just SDL.Quit -> exitSuccess
                   _ -> print event >> render window renderer glContext
                 loop
     loop
