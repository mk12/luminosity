-- Copyright 2012 Mitchell Kember

-- This module deliberately declares orphan instances:
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'JSON' instances for 'Scene' and all the other types it contains
-- (including 'Surface' and 'VectorT').
module Luminosity.Parse () where

import Control.Applicative ((<$>), (<*>))
import Text.JSON
    (JSON, JSValue(..), Result(..), fromJSObject, makeObj, readJSON, showJSON)
import qualified Data.Map as M

import Luminosity.Intersect
import Luminosity.Trace
import Luminosity.Vector

-- | Same as 'lookup' but generalized to any monad, and fails with a useful
-- error message.
--
-- > lookupM "a" [("b", 1)]  ==  fail "Missing element: a"
lookupM :: Monad m => String -> [(String, a)] -> m a
lookupM a as = maybe (fail $ "Missing element: " ++ a) return (lookup a as)

-- | Apply a predicate to a monadic value, and fail with the given string if the
-- predicate fails, otherwise keep the same value.
--
-- > return 5000 >>= ensure (> 0) "Failed."  ==  return 5000
-- > return (-1) >>= ensure (> 0) "Failed."  ==  fail "Failed."
ensure :: Monad m => (a -> Bool) -> String -> a -> m a
ensure p s x | p x       = return x
             | otherwise = fail s

instance (JSON a, Num a) => JSON (VectorT a) where
    showJSON (Vector x y z) = showJSON [x, y, z]
    readJSON v = case readJSON v of
        Ok [x, y, z] -> Ok $ Vector x y z
        Ok _         -> fail "Vector is not a 3-element array."
        Error s      -> fail s

instance JSON Scene where
    showJSON (Scene settings world camera objects lights materials) = makeObj
        [ ("settings", showJSON settings)
        , ("world", showJSON world)
        , ("camera", showJSON camera)
        , ("objects", showJSON objects)
        , ("lights", showJSON lights)
        , ("materials", showJSON materials) ]
    readJSON (JSObject obj) = Scene
        <$> f "settings" <*> f "world" <*> f "camera" <*> f "objects"
        <*> f "lights"   <*> f "materials" >>= ensure
        (\s -> all (`M.member` mMaterials s) $ map mMaterialID (mObjects s))
        "Reference to non-existant material-id."
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Scene must be an object."

instance JSON Settings where
    showJSON (Settings resolutionX resolutionY samples depth) = makeObj
        [ ("resolution-x", showJSON resolutionX)
        , ("resolution-y", showJSON resolutionY)
        , ("samples", showJSON samples)
        , ("depth", showJSON depth) ]
    readJSON (JSObject obj) = Settings
        <$> (f "resolution-x" >>= ensure (> 0) "resolution-x must be > 0.")
        <*> (f "resolution-y" >>= ensure (> 0) "resolution-y must be > 0.")
        <*> (f "samples" >>= ensure (>= 0) "samples must be >= 0.")
        <*> (f "depth" >>= ensure (> 0) "depth must be > 0.")
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Settings must be an object."

instance JSON World where
    showJSON (World sky) = makeObj [ ("sky", showJSON sky) ]
    readJSON (JSObject obj) = World <$> f "sky"
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "World must be an object."

instance JSON Camera where
    showJSON (Orthographic (Ray x v) upward orthoScale) = makeObj
        [ ("projection", showJSON "orthographic")
        , ("position", showJSON x)
        , ("direction", showJSON v)
        , ("upward", showJSON upward)
        , ("ortho-scale", showJSON orthoScale) ]
    showJSON (Perspective (Ray x v) upward focalLength) = makeObj
        [ ("projection", showJSON "perspective")
        , ("position", showJSON x)
        , ("direction", showJSON v)
        , ("upward", showJSON upward)
        , ("focal-length", showJSON focalLength) ]
    readJSON (JSObject obj) = case f "projection" of
        Ok "orthographic" -> Orthographic
            <$> (Ray <$> f "position"
            <*> fmap normalize (f "direction"))
            <*> fmap normalize (f "upward") <*> f "ortho-scale" >>= g
        Ok "perspective"  -> Perspective
            <$> (Ray <$> f "position"
            <*> fmap normalize (f "direction"))
            <*> fmap normalize (f "upward") <*> f "focal-length" >>= g
        Ok _              -> fail "Invalid Camera projection."
        Error s           -> fail s
      where
         f x = lookupM x (fromJSObject obj) >>= readJSON
         g = ensure (\c -> let (Ray _ d) = mSight c in d <.> mUpward c == 0)
            "Camera direction and upward vectors must be orthogonal."
    readJSON _ = fail "Camera must be an object."

instance JSON Object where
    showJSON (Object surface materialID) = makeObj
        [ ("surface", showJSON surface)
        , ("material-id", showJSON materialID) ]
    readJSON (JSObject obj) = Object <$> f "surface" <*> f "material-id"
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Object must be an object."

instance JSON Surface where
    showJSON (Sphere c r) = makeObj
        [ ("type", showJSON "sphere")
        , ("position", showJSON c)
        , ("radius", showJSON r) ]
    showJSON (Plane n d) = makeObj
        [ ("type", showJSON "plane")
        , ("normal", showJSON n)
        , ("distance", showJSON d) ]
    readJSON (JSObject obj) = case f "type" of
        Ok "sphere" -> Sphere <$> f "position" <*> f "radius"
        Ok "plane"  -> Plane  <$> fmap normalize (f "normal") <*> f "distance"
        Ok _        -> fail "Invalid Surface type."
        Error s     -> fail s
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Surface must be an object."

instance JSON Light where
    showJSON (Light position intensity) = makeObj
        [ ("position", showJSON position)
        , ("intensity", showJSON intensity) ]
    readJSON (JSObject obj) = Light <$> f "position" <*> f "intensity"
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Light must be an object."

instance JSON Material where
    showJSON (Material diffuse reflect) = makeObj
        [ ("diffuse", showJSON diffuse)
        , ("reflect", showJSON reflect) ]
    readJSON (JSObject obj) = Material <$> f "diffuse" <*> f "reflect"
        where f x = lookupM x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Material must be an object."
