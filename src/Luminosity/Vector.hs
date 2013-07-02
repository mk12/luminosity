-- Copyright 2012 Mitchell Kember. Subject to the MIT License.

-- | A simple and lightweight module for vector math. The operations satisfy all
-- the Euclidean vector properties
-- (<http://en.wikipedia.org/wiki/Euclidean_vector#Basic_properties>).
module Luminosity.Vector
(
-- * Scalar and vector
  Scalar
, Vector
-- * Vector type
, VectorT(..)
-- * Common vectors
, vzero
, unitX
, unitY
, unitZ
-- * Operators
, (<+>)
, (<->)
, (*>)
, (<.>)
, (><)
-- * Norm
, norm
, normSq
, normalize
-- * Negation
, vnegate
) where

import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad.Instances ()
import Data.Monoid (Monoid, mappend, mempty)

-- | A floating-point scalar value. This creates an abstraction layer over the
-- underlying numerical type used application-wide.
type Scalar = Double

-- | A floating-point vector value. This is meant to be used in conjunction with
-- 'Scalar' to provide the same abstraction for vectors, and to ensure that
-- both use the same floating-point type.
type Vector = VectorT Scalar

-- | A three-dimensional Euclidean vector. 
data VectorT a = Vector !a !a !a deriving (Eq, Bounded, Read, Show)

instance Functor VectorT where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative VectorT where
    pure x = Vector x x x
    (Vector f g h) <*> (Vector x y z) = Vector (f x) (g y) (h z)

instance Num a => Monoid (VectorT a) where
    mempty  = vzero
    mappend = (<+>)

-- | Calculate the sum of the components of a vector. This is not useful
-- mathematically, but it is used in the implementation of other functions.
vsum :: Num a => VectorT a -> a
vsum (Vector x y z) = x + y + z

-- | The null vector or zero vector.
vzero :: Num a => VectorT a
vzero = Vector 0 0 0

-- | The x-axis unit vector.
unitX :: Num a => VectorT a
unitX = Vector 1 0 0

-- | The y-axis unit vector.
unitY :: Num a => VectorT a
unitY = Vector 0 1 0

-- | The z-axis unit vector.
unitZ :: Num a => VectorT a
unitZ = Vector 0 0 1

-- Operator fixities
infixl 6 <+>, <->
infixl 7 *>
infixl 8 <.>
infixl 9 ><

-- | Vector addition. Add two vectors component-wise.
(<+>) :: Num a => VectorT a -> VectorT a -> VectorT a
(<+>) = (<*>) . fmap (+)

-- | Vector subtraction. Subtract one vector from another component-wise.
(<->) :: Num a => VectorT a -> VectorT a -> VectorT a
(<->) = (<*>) . fmap (-)

-- | Scalar multiplication. Multiply each component of a vector by a scalar.
(*>) :: Num a => a -> VectorT a -> VectorT a
(*>) = fmap . (*)

-- | Dot product (a.k.a. scalar product or inner product).
(<.>) :: Num a => VectorT a -> VectorT a -> a
(<.>) = (vsum .) . (<*>) . fmap (*)

-- | Cross product (a.k.a. vector product or outer product).
(><) :: Num a => VectorT a -> VectorT a -> VectorT a
(Vector x1 y1 z1) >< (Vector x2 y2 z2) = Vector
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

-- | Calculate the norm (a.k.a. length or magnitude) of a vector using the
-- Pythagorean theorem.
norm :: Floating a => VectorT a -> a
norm = sqrt . normSq

-- | Calculate the square of the norm of a vector. When comparing the lengths
-- of vectors, compare by the squared norm rather than by the norm. This is more
-- efficient because it skips the square root calculation.
normSq :: Num a => VectorT a -> a
normSq = vsum . fmap (^ 2)

-- | Normalize a vector, i.e. make it a unit vector.
normalize :: Floating a => VectorT a -> VectorT a
normalize = flip (/) . norm >>= fmap

-- | Negate a vector.
vnegate :: Num a => VectorT a -> VectorT a
vnegate = fmap negate
