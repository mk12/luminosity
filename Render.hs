-- Copyright 2012 Mitchell Kember.

module Render where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

import Trace
import Vector

type Pixel = (Scalar, Scalar)

-- Calculate the ray to be launched through a given pixel from a camera.
-- Non-integral pixel coordinates can be used for oversampling.
-- Note: the direction of the camera's sight ray and its upward vector must
-- be normalized.
rayForPixel :: Camera -> Settings -> Pixel -> Ray
rayForPixel (Orthographic (Ray c look) up scale) settings (x, y) = Ray
    (c <+> v *> up <+> u *> normalize (up >< look)) look
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    k      = scale / 2
    v      = k - scale * (y + 0.5) / resY
    u      = -k * resX / resY + scale * (x + 0.5) / resY
rayForPixel (Perspective (Ray c look) up length) settings (x, y) = Ray
    start (normalize $ start <-> focus)
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    v      = 0.5 - (y + 0.5) / resY
    u      = (x + 0.5 - resX / 2) / resY
    start  = c <+> v *> up <+> u *> normalize (up >< look)
    focus  = c <-> length *> look

render :: Scene -> Put
render s = do
    forM_ pixels (\xy -> putWord24be
  where
    (Settings resX resY samples _) = mSettings s
    pixels = init . zip $ [0..resX] [0..resY]
