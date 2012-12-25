module NC.SDLHelper where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

loadImage :: String -> IO Surface
loadImage filename = load filename >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

surfaceRect :: Surface -> Rect
surfaceRect surf = Rect 0 0 w h
 where w = surfaceGetWidth surf
       h = surfaceGetHeight surf
