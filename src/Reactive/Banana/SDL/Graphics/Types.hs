{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Types for graphics handling
module Reactive.Banana.SDL.Graphics.Types where

-- import           Data.Lens.Common
import           Data.Lens.Template
import           Graphics.UI.SDL  as SDL
-- import           Graphics.UI.SDL.TTF
import           Graphics.UI.SDL.TTF.Types 
-- import           Reactive.Banana  as R

-- | alias for surface
type Screen = SDL.Surface

-- | alias for an operation to draw on a surface, returning the drawn rectangle area if relevant
newtype Graphic = Graphic { paintGraphic :: Screen -> IO (Maybe Rect) }
-- | graphic operation on a rectangle
type GraphicOpt = Rect -> Graphic
-- | Graphic update
type GraphicUpdate = (GraphicOpt, Rect)
-- | Alignment
data Alignment=Start | Middle | End
-- | Mask coordinates and optional clipping rectangle
data Mask = Mask { _maskClip :: Maybe Rect, _maskX :: Int, _maskY :: Int }
-- | color fill
data Fill = Fill { _fillClip :: Rect, _fillColor :: Color }
-- | Standard Text
data Text = Text { _textMsg :: String, _textFont :: TTFFont, _textColor :: Color }
-- | Aligned Text
data AlignedText = AlignedText {_atextText :: Text, _atextHAlign :: Alignment, _atextVAlign :: Alignment}
-- | Image
data Image = Image { _imagePath :: String}
-- | a preloaded image
data LoadedImage = LoadedImage { _imageScreen :: Screen}

$(makeLenses [''Mask, ''Fill, ''Image, ''Text, ''AlignedText, ''LoadedImage])
-- manual lenses because template haskell doesn't work in a cross compiling environment
--maskClip :: Lens Mask (Maybe Rect)
--maskClip = lens _maskClip (\x s -> s { _maskClip = x})
--maskX :: Lens Mask Int
--maskX = lens _maskX (\x s -> s { _maskX = x})
--maskY :: Lens Mask Int
--maskY = lens _maskY (\x s -> s { _maskY = x})
--fillClip :: Lens Fill (Maybe Rect)
--fillClip = lens _fillClip (\x s -> s { _fillClip = x})
--fillColor :: Lens Fill Color
--fillColor = lens _fillColor (\x s -> s { _fillColor = x})
--imagePath :: Lens Image String
--imagePath = lens _imagePath (\x s -> s { _imagePath = x})
--imageScreen :: Lens LoadedImage Screen
--imageScreen = lens _imageScreen (\x s -> s { _imageScreen = x})

instance Eq Color where
    (Color r1 g1 b1 a1) == (Color r2 g2 b2 a2) = r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2

instance Show Color where
    show (Color r g b a) = "Color { " ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++ show a ++ " }"

-- | the draw class for involved graphics
class Draw canvas mask where
    draw :: canvas -> mask -> Graphic
