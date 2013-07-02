-- Copyright 2012 Mitchell Kember. Subject to the MIT License.

-- | The 'render' function and camera projection math. This module is
-- responsible for tranforming scenes into rendered images ready to be exported.
module Luminosity.Render (render) where

import Luminosity.Colour (Colour24, saturation, sRGB24)
import Luminosity.Vector
import Luminosity.Intersect (Ray(..))
import Luminosity.Misc (for)
import Luminosity.Trace

-- | An @(x,y)@ pair to represent a pixel. The coordinates are not integral
-- because they can also be subpixel coordinates.
type Pixel = (Scalar, Scalar)

-- | Calculate the ray to be launched through a pixel from a camera.
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

-- | Render a scene to a list of 24-bit colours in the usual order (row by
-- row, top to bottom).
render :: Scene -> [Colour24]
render scene = for pixels
    $ sRGB24
    . saturation
    . trace scene
    . rayForPixel (mCamera scene) ss
  where
    ss     = mSettings scene
    rows   = [0..fromIntegral (mResolutionY ss) - 1]
    cols   = [0..fromIntegral (mResolutionX ss) - 1]
    pixels = [(x, y) | y <- rows, x <- cols]
