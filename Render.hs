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

type Pixel = (Int, Int)

data Scene = Scene
    { mSettings  :: Settings
    , mWorld     :: World
    , mCamera    :: Camera
    , mObjects   :: [Object]
    , mLights    :: [Light]
    , mMaterials :: M.Map String Material
    } deriving (Eq, Show)

data Settings = Settings
    { mWidth   :: Int
    , mHeight  :: Int
    , mSamples :: Int
    , mDepth   :: Int
    } deriving (Eq, Show)

data World = World
    { mSky :: Colour
    } deriving (Eq, Show)

data Camera = Camera
    { mType :: String
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

rayForPixel :: Camera -> Pixel -> Ray
rayForPixel _ (x, y) = Ray (Vector (fromIntegral x) (fromIntegral y) 0) unitZ

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
            reflected = Ray x $ d <-> (2 * (d <.> n)) *> n
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








{-
-- Trace a ray through a scene and compute the final colour for that ray.
trace :: Scene -> Settings -> Ray -> Colour
trace scene settings ray = trace' (mObjects scene) (mLights scene) ray
    (mDepth settings) 1.0 (mSky settings)

trace' :: Scene -> Ray -> Int -> Double -> Colour -> Colour
trace' _ _ 0 _ colour = colour
trace' scene@(Scene objs lights mats) ray@(Ray _ d) level coef colour
    = case fstIntersection objs ray of
        Nothing       -> colour
        Just (obj, t) -> let
            x = extend t ray
            n = normal (mSurface obj) x
            mat = mMaterial obj
            cs = map ((fmap (* coef)) . applyLight mat x n objs) $ lights
            reflected = Ray x $ d <-> (2 * (d <.> n)) *> n
            in trace' scene reflected (level - 1)
                (coef * mReflect (mMaterial obj)) (foldr mappend colour cs)
-}










-- intersections = flip $ mapMaybe . liftM2 fmap (,) . flip (intersect . mSurface)
-- :pf \r xs -> mapMaybe (\x -> liftM ((,) x) (intersect r (mSurface x))) xs
-- intersections = mapMaybe . liftM2 fmap (,) . (. mSurface) . intersect
