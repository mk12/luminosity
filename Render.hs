-- Copyright 2012 Mitchell Kember.

module Render where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.Binary (Binary, Put, get, put)
import Data.Word (Word8)
-- import qualified Data.ByteString.Lazy as BL

-- ()
import Colour
import Intersect
import Trace
import Vector

type Pixel = (Scalar, Scalar)

instance (Binary a) => Binary (VectorT a) where
    put (Vector x y z) = put x >> put y >> put z
    get = Vector <$> get <*> get <*> get

-- Calculate the ray to be launched through a given pixel from a camera.
-- Non-integral pixel coordinates can be used for oversampling.
rayForPixel :: Camera -> Settings -> Pixel -> Ray
rayForPixel (Orthographic (Ray c look) up scale) settings (x, y)
    = Ray (c <+> v *> up <+> u *> normalize (up >< look)) look
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    k      = scale / 2
    v      = k - scale * (y + 0.5) / resY
    u      = -k * resX / resY + scale * (x + 0.5) / resY
rayForPixel (Perspective (Ray c look) up length) settings (x, y)
    = Ray start (normalize $ start <-> focus)
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    v      = 0.5 - (y + 0.5) / resY
    u      = (x + 0.5 - resX / 2) / resY
    start  = c <+> v *> up <+> u *> normalize (up >< look)
    focus  = c <-> length *> look

-- Render a scene out to a bytestring, each pixel being 24 bits.
render :: Scene -> Put
render scene = forM_ rows $ \y -> forM_ cols $ \x ->
    put . sRGB24 . trace scene . rayForPixel (mCamera scene) ss $ (x, y)
  where
    ss   = mSettings scene
    rows = init [0..(fromIntegral $ mResolutionY ss)]
    cols = init [0..(fromIntegral $ mResolutionX ss)]
