-- Copyright 2012 Mitchell Kember

-- | The 'Scene' type, several other types that it contains, and the 'trace'
-- function for ray tracing scenes. This module is responsible for transforming
-- a single ray into a colour.
module Luminosity.Trace
(
-- * Types
  Scene(..)
, Settings(..)
, World(..)
, Camera(..)
, Object(..)
, Light(..)
, Material(..)
-- * Ray tracing
, trace
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Ord (comparing)
import qualified Data.Map as M

import Luminosity.Colour (Colour)
import Luminosity.Vector
import Luminosity.Intersect
import Luminosity.Misc (maybeMinBy)

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

-- | Calculate all the intersections made by a ray with a list of objects, and
-- with each object return the point of intersection's distance from the ray's
-- initial point.
intersections :: [Object] -> Ray -> [(Object, Scalar)]
intersections xs r = mapMaybe (\x -> fmap ((,) x) (mSurface x `intersect` r)) xs

-- | Calculate the nearest intersection made by a ray with a list of objects,
-- and with each object return the point of intersection's distance from the
-- ray's initial point. Returns 'Nothing' if no intersections were made.
fstIntersection :: [Object] -> Ray -> Maybe (Object, Scalar)
fstIntersection = (maybeMinBy (comparing snd) .) . intersections

-- | Trace a ray through a scene and compute its colour.
trace :: Scene -> Ray -> Colour
trace scene ray = trace' scene ray (mDepth $ mSettings scene)

-- | Recursive ray tracing implementation used by 'trace'.
trace' :: Scene -> Ray -> Int -> Colour
trace' _ _ 0 = mempty
trace' scene@(Scene _ world _ objs lights mats) ray@(Ray _ v) level
    = case fstIntersection objs ray of
        Nothing       -> mSky world
        Just (obj, t) -> let
            mat  = mats M.! mMaterialID obj
            x'   = extend t ray
            n    = normal (mSurface obj) x'
            col  = mconcat . map (lighting mat x' n objs) $ lights
            ray' = Ray x' $ normalize $ v <-> 2 * v <.> n *> n
            ref  = mReflect mat
            in if ref == 0 then col
                else col <> ref *> trace' scene ray' (level - 1)

-- | Compute the colour that a light source contributes at a particular point.
lighting
    :: Material  -- ^ The material of the object which the point is on.
    -> Vector    -- ^ The position vector of the point.
    -> Vector    -- ^ The normal vector of the point.
    -> [Object]  -- ^ The objects in the scene (for tracing shadows).
    -> Light     -- ^ The light source.
    -> Colour    -- ^ The colour that the light source contributes.
lighting mat p n objs (Light x c)
    | lambert <= 0 || not (null ints) = mempty
    | otherwise = (*) <$> lambert *> c <*> mDiffuse mat
  where
    shadow  = normalize $ x <-> p
    lambert = shadow <.> n
    dist    = norm $ x <-> p
    ints    = filter ((<= dist) . snd)
            $ intersections objs (Ray p shadow)
