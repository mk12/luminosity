-- Copyright 2012 Mitchell Kember.

module Vector
( Scalar, Vector
, VectorT(..)
, (<+>), (<->)
, (*>)
, (<.>), (><)
, mag, magSqr
, vnegate, normalize
, vzero
, unitX, unitY, unitZ
) where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad.Instances
import Data.Monoid (Monoid, mappend, mempty)

type Scalar = Double
type Vector = VectorT Scalar

data VectorT a = Vector !a !a !a deriving(Eq, Show)

instance Functor VectorT where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative VectorT where
    pure x = Vector x x x
    (Vector f g h) <*> (Vector x y z) = Vector (f x) (g y) (h z)

instance (Num a) => Monoid (VectorT a) where
    mempty  = vzero
    mappend = (<+>)

-- Calculate the sum of the components of a vector. This is not directly useful
-- mathematically, but it is used to implement other operators.
vsum :: (Num a) => VectorT a -> a
vsum (Vector x y z) = x + y + z

-- Add and subtract two vectors component-wise.
infixl 6 <+>, <->
(<+>), (<->) :: (Num a) => VectorT a -> VectorT a -> VectorT a
(<+>) = (<*>) . fmap (+)
(<->) = (<*>) . fmap (-)

-- Scale a vector by a scalar value, that is multiply each component of the
-- vector by the scalar.
infixl 7 *>
(*>) :: (Num a) => a -> VectorT a -> VectorT a
(*>) = fmap . (*)

-- Calculate the dot product, that is the scalar product, of two vectors.
infixl 8 <.>
(<.>) :: (Num a) => VectorT a -> VectorT a -> a
(<.>) = (vsum .) . (<*>) . fmap (*)

-- Calculate the cross product of two vectors.
infixl 9 ><
(><) :: (Num a) => VectorT a -> VectorT a -> VectorT a
(Vector x1 y1 z1) >< (Vector x2 y2 z2) = Vector
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

-- Calculate the magnitude, that is the length, of a vector.
mag :: (Floating a) => VectorT a -> a
mag = sqrt . magSqr

-- Calculate the square of a vector's magnitude. This is more efficient as it
-- skips the square root computation. In particular, when vectors are sorted by
-- magnitude, it is much more efficient to sort by the squared magnitude.
magSqr :: (Num a) => VectorT a -> a
magSqr = vsum . fmap (^ 2)

-- Negate a vector (equivalent to multiplying it by -1). The negated vector has
-- the same magnitude as the original, but points in the opposite direction.
vnegate :: (Num a) => VectorT a -> VectorT a
vnegate = fmap negate

-- Normalize a vector, that is make its magnitude equal to one.
normalize :: (Floating a) => VectorT a -> VectorT a
normalize = flip (/) . mag >>= fmap

-- The zero (null) vector and the unit vectors.
vzero, unitX, unitY, unitZ :: (Num a) => VectorT a
vzero = Vector 0 0 0
unitX = Vector 1 0 0
unitY = Vector 0 1 0
unitZ = Vector 0 0 1
