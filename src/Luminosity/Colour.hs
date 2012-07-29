-- Copyright 2012 Mitchell Kember

-- | An extension to "Luminosity.Vector" for the more specific usage of
-- vectors to represent colours, with some useful operations on them.
module Luminosity.Colour
(
-- * Type synonyms
  Colour
, Colour24
, ColourT
-- * Re-exported types
, VectorT(..)
-- * Functions
, saturation
, sRGB24
) where

import Data.Word (Word8)

import Luminosity.Vector (VectorT(..))

-- | A floating-point colour.
type Colour = ColourT Double

-- | A 24-bit colour, i.e. a colour which has 8 bits per channel.
type Colour24 = ColourT Word8

-- | A three-channel colour, in other words a vector in a colour space.
type ColourT = VectorT

-- | Clip a colour to a maximum intensity of @1@ in each channel.
saturation :: (Fractional a, Ord a) => ColourT a -> ColourT a
saturation = fmap $ min 1

-- | Convert a single value from linear RGB space to sRGB using gamma encoding.
sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * x ** (1 / 2.4) - 0.055

-- | Convert a floating-point linear RGB colour to a 24-bit sRGB colour. All
-- channels should be in the range @[0,1]@, otherwise they will be clipped.
sRGB24 :: RealFloat a => ColourT a -> Colour24
sRGB24 = fmap $ round . max 0 . min 255 . (* 255) . sRGB
