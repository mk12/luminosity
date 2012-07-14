-- Copyright 2012 Mitchell Kember.

module Colour
( Colour
, Colour24
, ColourT
, sRGB24
) where

import Data.Word (Word8)

import Vector (VectorT)

type Colour   = ColourT Double
type Colour24 = ColourT Word8

-- An RGB colour, a three-dimensional vector in a colour space.
type ColourT  = VectorT

-- Convert a single value from linear RGB space to sRGB using gamma encoding.
sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * x ** (1 / 2.4) - 0.055

-- Convert a floating-point linear RGB colour to a 24-bit sRGB colour.
sRGB24 :: (RealFloat a) => ColourT a -> Colour24
-- TODO: implement an exposure operator (tone mapping) rather than
-- using a saturation operator (clipping: min 1).
-- (max 0 . min 255 is just a precaution to avoid colours wrapping/inverting)
sRGB24 = fmap $ round . max 0 . min 255 . (* 255) . sRGB . min 1
