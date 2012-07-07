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

-- An arrow in space represented by an origin vector and a direction vector,
-- in other words a line which extends to infinite in only one direction.
data Ray = Ray Vector Vector deriving (Eq, Show)

-- A geometric representation which a ray can intersect with.
data Surface = Sphere Vector Scalar
             | Plane  Vector Vector
             deriving (Eq, Show)

-- Extremely small value to prevent problems caused by floating point error.
epsilon :: (Fractional a) => a
epsilon = 10e-10

-- Calculate the location of a point on a ray given an offset from its origin.
extend :: Scalar -> Ray -> Vector
extend t (Ray x d) = x <+> t *> d

-- Calculate the normal vector of a point on a surface, where both the
-- surface's position and the given point are relative to the same origin.
normal :: Surface -> Vector -> Vector
normal (Sphere c _) x = normalize (x <-> c)
normal (Plane  _ n) _ = n

-- Calculate the closest point of intersection of a surface and ray, expressed
-- as the distance along the ray from the starting point.
-- Note: The direction vector of the ray must be normalized.
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
    q    | b < 0  = (-root - b) / 2
         | b >= 0 = ( root - b) / 2
    ts   = [q, c / q]
intersect (Plane p n) (Ray x d)
    | dot >= 0  = Nothing
    | otherwise = Just $ n <.> (p <-> x) / dot
  where
    dot = d <.> n
