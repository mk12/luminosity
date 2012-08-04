-- Copyright 2012 Mitchell Kember. Subject to the MIT license.

-- | The 'Ray' and 'Surface' types, with some useful operations on them (notably
-- the 'intersect' function).
module Luminosity.Intersect
(
-- * Ray and surface types
  Ray(..)
, Surface(..)
-- * Functions
, extend
, normal
, intersect
) where

import Data.List (sort)
import Data.Maybe (listToMaybe)

import Luminosity.Vector

-- | A half-line in three-dimensional Euclidean space. It consists of an initial
-- point (represented by a position vector) which extends infinitely in one
-- direction (represented by a unit vector).
--
-- Note: All functions assume that the direction vector is normalized.
data Ray = Ray Vector Vector  -- ^ Position and direction.
         deriving (Eq, Show)

-- | A simple geometrical solid in three-dimensional Euclidean space.
--
-- Note: All functions assume that the normal vector of a 'Plane' is normalized.
data Surface = Sphere Vector Scalar  -- ^ Position and radius.
             | Plane  Vector Scalar  -- ^ Normal and distance from origin.
             deriving (Eq, Show)

-- | An extremely small positive value used to prevent problems caused by
-- floating-point error.
epsilon :: Fractional a => a
epsilon = 1.0e-10

-- | Calculate the position vector of a point on a ray given its distance from
-- the ray's initial point.
extend :: Scalar -> Ray -> Vector
extend t (Ray x v) = x <+> t *> v

-- | Calculate the unit normal vector of a point on a surface.
normal :: Surface -> Vector -> Vector
normal (Sphere c _) x = normalize $ x <-> c
normal (Plane  n _) _ = n

-- | Calculate the closest point of intersection of a surface and ray, expressed
-- as the distance along the ray from the initial point (often denoted as the
-- parameter @t@). Returns 'Nothing' if they do not intersect.
intersect :: Surface -> Ray -> Maybe Scalar
intersect (Sphere p r) (Ray x v)
    | disc < 0  = Nothing
    | otherwise = listToMaybe . sort . filter (>= epsilon) $ ts
  where
    xo   = x <-> p
    b    = 2 * v <.> xo
    c    = normSq xo - r ^ 2
    disc = b ^ 2 - 4 * c
    root = sqrt disc
    q    | b < 0     = (-root - b) / 2
         | otherwise = ( root - b) / 2
    ts   = [q, c / q]
intersect (Plane n d) (Ray x v)
    | vn >= 0 || t < 0 = Nothing
    | otherwise        = Just t
  where
    vn = v <.> n
    t  = (vnegate x <.> n + d) / vn
