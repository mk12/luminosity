-- Copyright 2012 Mitchell Kember.

module Vector
( Scalar
, Vector
, VectorT(..)
, (<+>), (<->)
, (*>)
, (<.>), (><)
, norm, normSq
, vnegate
, normalize
, vzero
, unitX, unitY, unitZ
) where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad.Instances
import Data.Binary (Binary, get, put)
import Data.Monoid (Monoid, mappend, mempty)

-- Type synonyms for scalar and vector values to create an abstraction layer for
-- the underlying numerical type used application-wide.
type Scalar = Double
type Vector = VectorT Scalar

-- A three-dimensional Euclidean vector. 
data VectorT a = Vector !a !a !a deriving (Eq, Bounded, Read, Show)

instance (Binary a) => Binary (VectorT a) where
    put (Vector x y z) = put x >> put y >> put z
    get = Vector <$> get <*> get <*> get

instance Functor VectorT where
    fmap f (Vector x y z) = Vector (f x) (f y) (f z)

instance Applicative VectorT where
    pure x = Vector x x x
    (Vector f g h) <*> (Vector x y z) = Vector (f x) (g y) (h z)

instance (Num a) => Monoid (VectorT a) where
    mempty  = vzero
    mappend = (<+>)

-- Calculate the sum of the components of a vector. This is not directly useful
-- mathematically, but it is useful for the implementation of other functions.
vsum :: (Num a) => VectorT a -> a
vsum (Vector x y z) = x + y + z

-- Add or subtract two vectors component-wise. Vector subtraction is equivalent
-- to negating the subtrahend and performing vector addition. Vector addition is
-- commutative and associative, while vector subtraction is anticommutative and
-- left-associative. Both have the null/zero vector as the identity element.
infixl 6 <+>, <->
(<+>), (<->) :: (Num a) => VectorT a -> VectorT a -> VectorT a
(<+>) = (<*>) . fmap (+)
(<->) = (<*>) . fmap (-)

-- Multiply, or re-scale, a vector by a scalar. Each component of the vector is
-- multiplied by the scalar (the scale factor). The resulting vector has the
-- same direction as the original (unless the scale factor is negative, in which
-- case it has the opposite direction), and the norm is multiplied by the scalar
-- as well. Scalar multiplication is distributive.
infixl 7 *>
(*>) :: (Num a) => a -> VectorT a -> VectorT a
(*>) = fmap . (*)

-- Calculate the dot product (a.k.a. inner product or scalar product) of two
-- vectors. It is equal to the cosine of the angle between the vectors
-- multiplied with their norms. The dot product of a vector with itself is equal
-- to its norm squared. When two vectors are orthogonal, their dot product is
-- always zero. The dot product is commutative and distributive.
infixl 8 <.>
(<.>) :: (Num a) => VectorT a -> VectorT a -> a
(<.>) = (vsum .) . (<*>) . fmap (*)

-- Calculate the cross product (a.k.a. outer product or vector product) of two
-- vectors. The resulting vector is perpendicular to both of the vectors being
-- multiplied and therefore normal to the plane containing them. The cross
-- product is anticommutative, distributive, and non-associative.
infixl 9 ><
(><) :: (Num a) => VectorT a -> VectorT a -> VectorT a
(Vector x1 y1 z1) >< (Vector x2 y2 z2) = Vector
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

-- Calculate the norm (a.k.a. length or magnitude) of a vector using the
-- Pythagorean theorem. The norm represents the length of the vector. The norm
-- is always positive and nonzero (except for the norm of the null/zero vector).
norm :: (Floating a) => VectorT a -> a
norm = sqrt . normSq

-- Calculate the square of the norm of a vector. This is equal to the dot
-- product of the vector with itself. When comparing the lengths of vectors,
-- an easy optimization is to compare by the squared norm rather than by the
-- norm. This is more efficient because it skips the square root calculation.
normSq :: (Num a) => VectorT a -> a
normSq = vsum . fmap (^ 2)

-- Negate a vector, equivalent to multiplying it by the scalar -1. The resulting
-- vector has the same norm as the original, but has the opposite direction (it
-- flips around by an angle of 180 degrees). The negated vector can also be
-- defined as the original's additive inverse.
vnegate :: (Num a) => VectorT a -> VectorT a
vnegate = fmap negate

-- Normalize a vector, that is make it a unit vector (a vector whose norm is
-- equal to 1). The resulting vector has the same direction as the original.
-- A unit vector essentially represents only direction, and has no meaningful
-- magnitude associated with it. Normalization and scalar multiplication can be
-- used together to change a vector's norm.
normalize :: (Floating a) => VectorT a -> VectorT a
normalize = flip (/) . norm >>= fmap

-- The null/zero vector and the axis unit vectors.
vzero, unitX, unitY, unitZ :: (Num a) => VectorT a
vzero = Vector 0 0 0
unitX = Vector 1 0 0
unitY = Vector 0 1 0
unitZ = Vector 0 0 1
