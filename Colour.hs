-- Copyright 2012 Mitchell Kember.

module Colour
( Colour
, Colour24
, sRGB24
) where

import Data.Word (Word8)

import Vector (VectorT)

type Colour   = VectorT Double
type Colour24 = VectorT Word8

-- A colour is a vector in a colour space.
type ColourT  = VectorT

-- Convert a value (colour component) from linear RGB space to sRGB.
sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * (x ** (1 / 2.4)) - 0.055

-- Convert a linear RGB colour to a 24-bit sRGB colour.
sRGB24 :: (RealFloat a) => ColourT a -> Colour24
sRGB24 = fmap (round . (* 255) . sRGB)
