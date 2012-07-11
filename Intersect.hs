-- Copyright 2012 Mitchell Kember.

module Intersect
( Ray(..)
, Surface(..)
, extend
, normal
, intersect
) where

import Data.List (sort)
import Data.Maybe (listToMaybe)

import Vector

-- A Euclidean half-line with an initial point (represented by a positon vector)
-- which extends infinitely in one direction (represented by a unit vector).
-- Note: All functions assume that the direction vector is normalized.
data Ray = Ray Vector Vector deriving (Eq, Show)

-- A simple geometrical solid in three-dimensional Euclidean space.
data Surface = Sphere Vector Scalar
             | Plane  Vector Vector
             deriving (Eq, Show)

-- An extremely small value to prevent problems caused by floating point error.
epsilon :: (Fractional a) => a
epsilon = 1.0e-10

-- Calculate the position of a point on a ray given an offset from its origin.
extend :: Scalar -> Ray -> Vector
extend t (Ray x d) = x <+> t *> d

-- Calculate the normal unit vector of a point on a surface.
normal :: Surface -> Vector -> Vector
normal (Sphere c _) x = normalize (x <-> c)
normal (Plane  _ n) _ = n

-- Calculate the closest point of intersection of a surface and ray, expressed
-- as the distance along the ray from the starting point.
intersect :: Surface -> Ray -> Maybe Scalar
intersect (Sphere p r) (Ray x d)
    | disc < 0  = Nothing
    | otherwise = listToMaybe . sort . filter (>= epsilon) $ ts
  where
    xo   = x <-> p
    b    = 2 * d <.> xo
    c    = xo <.> xo - r ^ 2
    disc = b ^ 2 - 4 * c
    root = sqrt disc
    q    | b < 0     = (-root - b) / 2
         | otherwise = ( root - b) / 2
    ts   = [q, c / q]
intersect (Plane p n) (Ray x d)
    | dot >= 0  = Nothing
    | otherwise = Just $ n <.> (p <-> x) / dot
  where
    dot = d <.> n
