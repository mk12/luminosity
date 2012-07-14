-- Copyright 2012 Mitchell Kember.

module Colour
( Colour
, Colour24
, ColourT
, saturation
, sRGB24
) where

import Data.Word (Word8)

import Vector (VectorT)

type Colour   = ColourT Double
type Colour24 = ColourT Word8

-- An RGB colour, a three-dimensional vector in a colour space.
type ColourT  = VectorT

-- Clip a colour to a maximum intensity of 1 in each channel.
saturation :: (Fractional a, Ord a) => ColourT a -> ColourT a
saturation = fmap $ min 1

-- Convert a single value from linear RGB space to sRGB using gamma encoding.
sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * x ** (1 / 2.4) - 0.055

-- Convert a floating-point linear RGB colour to a 24-bit sRGB colour. All
-- channels of the input colour should be in the range [0,1], or they will be
-- clipped. If necessary, the colour should have already undergone a saturation
-- or tone mapping operation--the clipping in this function exists only as a
-- precaution to avoid any error causing the values to wrap around.
sRGB24 :: (RealFloat a) => ColourT a -> Colour24
sRGB24 = fmap $ round . max 0 . min 255 . (* 255) . sRGB
