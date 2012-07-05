-- Copyright 2012 Mitchell Kember.

module Colour
( Colour
, RGB(..)
, sRGB24
) where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Data.Monoid (Monoid, mappend, mempty)
import Data.Word (Word8)

type Colour = RGB Double

data RGB a = RGB !a !a !a deriving (Eq, Show)

instance Functor RGB where
    fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Applicative RGB where
    pure x = RGB x x x
    (RGB f g h) <*> (RGB x y z) = RGB (f x) (g y) (h z)

instance (Num a) => Monoid (RGB a) where
    mempty  = RGB 0 0 0
    mappend = (<*>) . (fmap (+))

-- Convert a value (colour component) from linear RGB space to sRGB.
sRGB :: (Floating a, Ord a) => a -> a
sRGB x | x <= 0.0031308 = 12.92 * x
       | otherwise      = 1.055 * (x ** (1 / 2.4)) - 0.055

-- Convert a linear RGB colour to a 24-bit sRGB colour.
sRGB24 :: (RealFloat a) => RGB a -> RGB Word8
sRGB24 = fmap (round . (* 255) . sRGB)
