{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Combinators for SDL graphics
module Reactive.Banana.SDL.Graphics.Util where

import           Reactive.Banana.SDL.Graphics.Types

import           Control.Monad                      (void)
import           Data.Lens.Common
import           Graphics.UI.SDL                    as SDL hiding (flip)
import qualified Graphics.UI.SDL                    as SDL (flip)
import           Graphics.UI.SDL.Image              (load)
import           Graphics.UI.SDL.TTF
import           Reactive.Banana                    as R
import           Reactive.Banana.Frameworks         (Frameworks, changes,
                                                     reactimate, reactimate')

-- | draw a Graphic over another
over :: Graphic -> Graphic -> Graphic
(Graphic x) `over` (Graphic y) = Graphic $ \surface -> y surface >> x surface

-- | draw a Graphic under another
under :: Graphic -> Graphic -> Graphic
under = flip over

-- | draw a Draw relative to the result rect of the graphic
inside :: (Draw c Mask) => c -> Graphic -> Graphic
(c) `inside` (Graphic x) = Graphic $ \surface -> do
    mask <- x surface
    let Graphic y = draw c (Mask Nothing (maybe - rectX mask) (maybe 0 rectY mask))
    y surface
    return mask

-- | empty graphic
emptyG :: Graphic
emptyG = Graphic $ \_ -> return Nothing

-- | render and swap
render :: Graphic -> Graphic
render (Graphic x) = Graphic $ \surface -> x surface >> SDL.flip surface >> return Nothing

withinBox :: Rect -> Graphic -> GraphicOpt
withinBox r g r' =
          if r `intersect` r'
             then emptyG
             else g

overOpt :: GraphicOpt -> GraphicOpt -> GraphicOpt
overOpt g1 g2 r = g1 r `over` g2 r

overUpdate :: GraphicOpt -> GraphicUpdate -> GraphicUpdate
overUpdate g1 (g2, r) = (g1 `overOpt` g2, r)

-- | do the two rectangles intersect?                       
intersect :: Rect -> Rect -> Bool
intersect r1 r2 = xintersect && yintersect
          where 
                xintersect = x1 `between` (x2, w2) || w1 `between` (x2, w2)
                yintersect = y1 `between` (y2, h2) || h1 `between` (y2, h2)
                x1 = rectX r1
                x2 = rectX e2
                y1 = rectY r1
                y2 = rectY r2
                w1 = x1 + rectW r1
                w2 = x2 + rectW r2
                h1 = y1 + rectH r1
                h2 = y2 + rectH r2

-- | is the given x in between the range
between :: Int -> (Int, Int) -> Bool
between x (l, h) = x >= l && x <= h


instance Draw SDL.Surface Mask where
         draw src mask = Graphic $ \dst -> blitSurface src clip dst offset >> return offset
             where
                 clip = maskClip ^$ mask
                 offset = Just Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW = 0, rectH = 0}

instance Draw Fill Mask where
         draw fill mask = Graphic $ \dst -> pixel dst >>= \c -> fillRect dst clip c >> Return Nothing
             where
                 pixel dst = (mapRGB . surfaceGetPixelFormat) dst (colorRed color) (colorGreen color) (colorBlue color)
                 clip = fillClip ^$ fill
                 color = fillColor ^$ fill

instance Draw Text Mask where
         draw text mask = Graphic $ \dst -> blitText dst
              where
                blitText dst = do
                  txt <- renderTextBlended (textFont ^$ text) (textMsg ^$ text) (textColor ^$ text)
                  blitSurface txt clip dst offset
                  freeSurface txt
                  return offset
                clip = maskClip ^$ mask
                offset = Just Rect { rectX = maskX ^$ mask, rectY = maskY ^$ mask, rectW  = 0, rectH = 0 }
