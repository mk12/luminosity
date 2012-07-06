-- Copyright 2012 Mitchell Kember.

-- Note: install Text.JSON with "cabal install json --ghc-options=-DMAP_AS_DICT"

module Parse where

import Control.Applicative ((<$>), (<*>))
import Text.JSON
    (JSON, JSValue(..), Result(..), fromJSObject, makeObj, readJSON, showJSON)

import Intersect
import Render
import Vector

mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

instance (JSON a, Num a) => JSON (VectorT a) where
    showJSON (Vector x y z) = showJSON [x, y, z]
    readJSON x = case readJSON x of
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
        <*> f "lights"   <*> f "materials"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Scene must be an object."

instance JSON Settings where
    showJSON (Settings width height samples depth) = makeObj
        [ ("width", showJSON width)
        , ("height", showJSON height)
        , ("samples", showJSON samples)
        , ("depth", showJSON depth) ]
    readJSON (JSObject obj) = Settings
        <$> f "width" <*> f "height" <*> f "samples" <*> f "depth"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Settings must be an object."

instance JSON World where
    showJSON (World sky) = makeObj [ ("sky", showJSON sky) ]
    readJSON (JSObject obj) = World <$> f "sky"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "World must be an object."

instance JSON Camera where
    showJSON (Camera thetype)  = makeObj [ ("type", showJSON thetype) ]
    readJSON (JSObject obj) = Camera <$> f "type"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Camera must be an object."

instance JSON Object where
    showJSON (Object surface materialid) = makeObj
        [ ("surface", showJSON surface)
        , ("material", showJSON materialid) ]
    readJSON (JSObject obj) = Object <$> f "surface" <*> f "material"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Object must be an object."

instance JSON Surface where
    showJSON (Sphere c r) = makeObj
        [ ("type", showJSON "sphere")
        , ("position", showJSON c)
        , ("radius", showJSON r) ]
    showJSON (Plane p n) = makeObj
        [ ("type", showJSON "plane")
        , ("position", showJSON p)
        , ("normal", showJSON n) ]
    readJSON (JSObject obj) = case (f "type") of
        Ok "sphere" -> Sphere <$> f "position" <*> f "radius"
        Ok "plane"  -> Plane  <$> f "position" <*> f "normal"
        Ok _        -> fail "Invalid Surface type."
        Error s     -> fail s
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Surface must be an object."

instance JSON Light where
    showJSON (Light position intensity) = makeObj
        [ ("position", showJSON position)
        , ("intensity", showJSON intensity) ]
    readJSON (JSObject obj) = Light <$> f "position" <*> f "intensity"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Light must be an object."

instance JSON Material where
    showJSON (Material diffuse reflect) = makeObj
        [ ("diffuse", showJSON diffuse)
        , ("reflect", showJSON reflect) ]
    readJSON (JSObject obj) = Material <$> f "diffuse" <*> f "reflect"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
    readJSON _ = fail "Material must be an object."
