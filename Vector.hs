-- Copyright 2012 Mitchell Kember.

module Vector
( Scalar
, Vector
, Vector3D(..)
, (<+>)
, (<->)
, (*>)
, (<.>)
, cross
, mag
, magSqr
, vnegate
, normalize
, vzero
, unitX
, unitY
, unitZ
) where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad.Instances
import Data.Monoid (Monoid, mappend, mempty)

type Scalar = Double
type Vector = Vector3D Scalar

data Vector3D a = XYZ !a !a !a deriving(Eq, Show)

instance Functor Vector3D where
   fmap f (XYZ x y z) = XYZ (f x) (f y) (f z)

instance Applicative Vector3D where
    pure x = XYZ x x x
    (XYZ f g h) <*> (XYZ x y z) = XYZ (f x) (g y) (h z)

instance (Num a) => Monoid (Vector3D a) where
    mempty  = vzero
    mappend = (<*>) . (fmap (+))

-- Calculate the sum of the components of a vector. This is not directly useful
-- mathematically, but is used to implement other operators.
vsum :: (Num a) => Vector3D a -> a
vsum (XYZ x y z) = x + y + z

-- Add and subtract two vectors component-wise.
infixl 6 <+>, <->
(<+>), (<->) :: (Num a) => Vector3D a -> Vector3D a -> Vector3D a
(<+>) = mappend
(<->) = (. vnegate) . mappend

-- Scale a vector by a scalar value, that is multiply each component of the
-- vector by the scalar.
infixl 8 *>
(*>) :: (Num a) => a -> Vector3D a -> Vector3D a
(*>) = fmap . (*)

-- Calculate the dot product, that is the scalar product, of two vectors.
infixl 8 <.>
(<.>) :: (Num a) => Vector3D a -> Vector3D a -> a
(<.>) = (vsum .) . ((<*>) . (fmap (*)))

-- Calculate the cross product of two vectors.
cross :: (Num a) => Vector3D a -> Vector3D a -> Vector3D a
cross (XYZ x1 y1 z1) (XYZ x2 y2 z2) = XYZ
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

-- Calculate the magnitude, that is the length, of a vector.
mag :: (Floating a) => Vector3D a -> a
mag = sqrt . magSqr

-- Calculate the square of a vector's magnitude. This is more efficient as it
-- skips the square root computation. In particular, when vectors are sorted by
-- magnitude, it is much more efficient to sort by the squared magnitude.
magSqr :: (Num a) => Vector3D a -> a
magSqr = vsum . fmap (^ 2)

-- Negate a vector, equivalent to multiplying it by -1. The negated vecto has
-- the same magnitude as the original, but has the opposite direction.
vnegate :: (Num a) => Vector3D a -> Vector3D a
vnegate = fmap negate

-- Normalize a vector, that is make it's magnitude equal to one.
normalize :: (Floating a) => Vector3D a -> Vector3D a
normalize = flip (/) . mag >>= fmap

vzero, unitX, unitY, unitZ :: (Num a) => Vector3D a
vzero = XYZ 0 0 0
unitX = XYZ 1 0 0
unitY = XYZ 0 1 0
unitZ = XYZ 0 0 1
