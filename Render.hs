-- Copyright 2012 Mitchell Kember.

module Render where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend, mempty)
import Data.Ord (comparing)
import qualified Data.Map as M

import Colour
import Intersect
import Misc (maybeMinBy)
import Vector

type Pixel = (Scalar, Scalar)

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

-- render :: Scene -> 

-- Calculate the ray to be launched through a given pixel from a camera.
-- Non-integral pixel coordinates can be used for oversampling.
rayForPixel :: Camera -> Settings -> Pixel -> Ray
rayForPixel (Orthographic (Ray c look) up scale) settings (x, y) = Ray
    (c <+> v *> up <+> u *> up >< look) look
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    k      = scale / 2
    v      = k - scale * (y + 0.5) / resY
    u      = -k * resX / resY + scale * (x + 0.5) / resY
rayForPixel (Perspective (Ray c look) up length) settings (x, y) = Ray
    start (normalize $ start <-> focus)
  where
    resX   = fromIntegral (mResolutionX settings)
    resY   = fromIntegral (mResolutionY settings)
    v      = 0.5 - (y + 0.5) / resY
    u      = (x + 0.5 - resX / 2) / resY
    start  = c <+> v *> up <+> u *> up >< look
    focus  = c <-> length *> look

-- Calculate all the intersections made by a ray with a list of objects, and
-- return the objects associated with the distance from the ray's origin.
intersections :: [Object] -> Ray -> [(Object, Scalar)]
intersections xs r = mapMaybe (\x -> fmap ((,) x) (intersect (mSurface x) r)) xs

-- Calculate the closest intersection made by a ray with a list of objects, and
-- return the object associated with the distance from the ray's origin.
fstIntersection :: [Object] -> Ray -> Maybe (Object, Scalar)
fstIntersection = (maybeMinBy (comparing snd) .) . intersections

-- Trace a ray through a scene and compute the final colour for that ray.
trace :: Scene -> Ray -> Colour
trace scene@(Scene s w _ _ _ _) ray = trace' scene ray (mDepth s) 1.0 (mSky w)

-- Recursive ray tracing implementation.
trace' :: Scene -> Ray -> Int -> Double -> Colour -> Colour
trace' _ _ 0 _ colour = colour
trace' scene@(Scene _ _ _ objs lights mats) ray@(Ray _ d) level coef colour
    = case fstIntersection objs ray of
        Nothing       -> colour
        Just (obj, t) -> let
            x = extend t ray
            n = normal (mSurface obj) x
            (Just mat) = M.lookup (mMaterialID obj) mats
            cs = map ((fmap (* coef)) . applyLight mat x n objs) $ lights
            reflected = Ray x $ normalize $ d <-> (2 * (d <.> n)) *> n
            in trace' scene reflected (level - 1) (coef * mReflect mat)
                (foldr mappend colour cs)

-- Compute the colour that a light contributes at a particular point.
applyLight :: Material -> Vector -> Vector -> [Object] -> Light -> Colour
applyLight mat point n objs (Light x c)
    | lambert <= 0 || shadow = mempty
    | otherwise = ((*) . (* lambert)) <$> c <*> (mDiffuse mat)
  where
    delta  = normalize (x <-> point)
    lambert = delta <.> n
    shadow  = not . null $ intersections objs (Ray point delta)
