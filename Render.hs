-- Copyright 2012 Mitchell Kember.

module Render (render) where

import Colour (Colour24, saturation, sRGB24)
import Intersect (Ray(..))
import Misc (for)
import Trace
import Vector

type Pixel = (Scalar, Scalar)

-- Calculate the ray to be launched through a given pixel from a camera.
-- Non-integral pixel coordinates can be used for taking subpixel samples.
rayForPixel :: Camera -> Settings -> Pixel -> Ray
rayForPixel (Orthographic (Ray c look) up scale) settings (x, y)
    = Ray (c <+> v *> up <+> u *> normalize (up >< look)) look
  where
    resX  = fromIntegral (mResolutionX settings)
    resY  = fromIntegral (mResolutionY settings)
    k     = scale / 2
    v     = k - scale * (y + 0.5) / resY
    u     = -k * resX / resY + scale * (x + 0.5) / resY
rayForPixel (Perspective (Ray c look) up len) settings (x, y)
    = Ray start (normalize $ start <-> focus)
  where
    resX  = fromIntegral (mResolutionX settings)
    resY  = fromIntegral (mResolutionY settings)
    v     = 0.5 - (y + 0.5) / resY
    u     = (x + 0.5 - resX / 2) / resY
    start = c <+> v *> up <+> u *> normalize (up >< look)
    focus = c <-> len *> look

-- Render a scene to a list of 24-bit colours, starting from the top left pixel,
-- going across each row, and ending at the bottom right pixel.
render :: Scene -> [Colour24]
render scene = for pixels
    $ sRGB24
    . saturation
    . trace scene
    . rayForPixel (mCamera scene) ss
  where
    ss     = mSettings scene
    rows   = [0..(fromIntegral $ mResolutionY ss) - 1]
    cols   = [0..(fromIntegral $ mResolutionX ss) - 1]
    pixels = [(x, y) | y <- rows, x <- cols]
