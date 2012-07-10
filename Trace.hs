-- Copyright 2012 Mitchell Kember.

module Trace (trace) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend, mempty)
import Data.Ord (comparing)
import qualified Data.Map as M

import Colour
import Intersect
import Misc (maybeMinBy)
import Vector

data Scene = Scene
    { mSettings  :: Settings
    , mWorld     :: World
    , mCamera    :: Camera
    , mObjects   :: [Object]
    , mLights    :: [Light]
    , mMaterials :: M.Map String Material
    } deriving (Eq, Show)

data Settings = Settings
    { mResolutionX :: Int
    , mResolutionY :: Int
    , mSamples     :: Int
    , mDepth       :: Int
    } deriving (Eq, Show)

data World = World
    { mSky :: Colour
    } deriving (Eq, Show)

data Camera = Orthographic
    { mSight       :: Ray
    , mUpward      :: Vector
    , mOrthoScale  :: Scalar
    }       | Perspective
    { mSight       :: Ray
    , mUpward      :: Vector
    , mFocalLength :: Scalar
    } deriving (Eq, Show)

data Object = Object
    { mSurface    :: Surface
    , mMaterialID :: String
    } deriving (Eq, Show)

data Light = Light
    { mPosition  :: Vector
    , mIntensity :: Colour
    } deriving (Eq, Show)

data Material = Material
    { mDiffuse :: Colour
    , mReflect :: Double
    } deriving (Eq, Show)

-- Trace a ray through a scene and compute the final colour for that ray.
trace :: Scene -> Ray -> Colour
trace scene ray = trace' scene ray (mDepth $ mSettings scene) 1.0 mempty

-- Calculate all the intersections made by a ray with a list of objects, and
-- return the objects associated with the distance from the ray's origin.
intersections :: [Object] -> Ray -> [(Object, Scalar)]
intersections xs r = mapMaybe (\x -> fmap ((,) x) (intersect (mSurface x) r)) xs

-- Calculate the closest intersection made by a ray with a list of objects, and
-- return the object associated with the distance from the ray's origin.
fstIntersection :: [Object] -> Ray -> Maybe (Object, Scalar)
fstIntersection = (maybeMinBy (comparing snd) .) . intersections

-- Recursively compute the colour for a ray.
trace' :: Scene -> Ray -> Int -> Double -> Colour -> Colour
trace' _ _ 0 _ colour = colour
trace' _ _ _ 0 colour = colour
trace' scene@(Scene _ w _ objs lights mats) ray@(Ray _ d) level coef colour
    = case fstIntersection objs ray of
        Nothing       -> mSky w
        Just (obj, t) -> let
            mat = mats M.! mMaterialID obj
            x   = extend t ray
            n   = normal (mSurface obj) x
            cs  = map (fmap (* coef) . applyLight mat x n objs) lights
            ref = Ray x $ normalize $ d <-> (2 * (d <.> n)) *> n
            in trace' scene ref (level - 1) (coef * mReflect mat)
                (foldr mappend colour cs)

-- Compute the colour that a light source contributes at a particular point.
applyLight :: Material -> Vector -> Vector -> [Object] -> Light -> Colour
applyLight mat point n objs (Light x c)
    | lambert <= 0 || shadow = mempty
    | otherwise = ((*) . (* lambert)) <$> c <*> (mDiffuse mat)
  where
    delta   = normalize (x <-> point)
    lambert = delta <.> n
    shadow  = not . null $ intersections objs (Ray point delta)
