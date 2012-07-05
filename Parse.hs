-- Copyright 2012 Mitchell Kember.

module Parse where

import Control.Applicative ((<$>), (<*>))
import Text.JSON

import Colour (RGB(..))
import Intersect (Surface(..))
import Render
import Vector (Vector3D(..))

mLookup a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

instance (JSON a, Num a) => JSON (Vector3D a) where
    showJSON (XYZ x y z) = showJSON [x, y, z]
    readJSON x = do
        [x, y, z] <- readJSON x
        return $ XYZ x y z

instance (JSON a, Num a) => JSON (RGB a) where
    showJSON (RGB r g b) = showJSON [r, g, b]
    readJSON x = do
        [r, g, b] <- readJSON x
        return $ RGB r g b

instance JSON Scene where
    showJSON x = makeObj
        [ ("settings", showJSON $ mSettings x)
        , ("world", showJSON $ mWorld x)
        , ("camera", showJSON $ mCamera x)
        , ("objects", showJSON $ mObjects x)
        , ("lights", showJSON $ mLights x) ]
    readJSON (JSObject obj) = do
        settings <- mLookup "settings" jsonObjAssoc >>= readJSON
        world <- mLookup "world" jsonObjAssoc >>= readJSON
        camera <- mLookup "camera" jsonObjAssoc >>= readJSON

        objects <- mLookup "objects" jsonObjAssoc >>= readJSON

        (JSObject lightsObj) <- mLookup "lights" jsonObjAssoc
        let lights = readJSON . JSArray . map snd . fromJSObject $ lightsObj

        return $ Scene settings world camera objects lights
        where jsonObjAssoc = fromJSObject obj

instance JSON Settings where
    showJSON x = makeObj
        [ ("width", showJSON $ mWidth x)
        , ("height", showJSON $ mHeight x)
        , ("samples", showJSON $ mSamples x)
        , ("depth", showJSON $ mDepth x) ]
    readJSON (JSObject obj) = Settings <$>
        f "width" <*> f "height" <*> f "samples" <*> f "depth"
        where f x = mLookup x (fromJSObject obj) >>= readJSON

instance JSON World where
    showJSON x = makeObj [ ("sky", showJSON $ mSky x) ]
    readJSON (JSObject obj) = World <$> f "sky"
        where f x = mLookup x (fromJSObject obj) >>= readJSON

instance JSON Camera where
    showJSON x = makeObj [ ("type", showJSON $ mType x) ]
    readJSON (JSObject obj) = Camera <$> f "type"
        where f x = mLookup x (fromJSObject obj) >>= readJSON



instance JSON Object where
    showJSON x = makeObj
        [ ("surface", showJSON $ mSurface x) ]




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
        where f x = mLookup x (fromJSObject obj) >>= readJSON

instance JSON Light where
    showJSON x = makeObj
        [ ("position", showJSON $ mPosition x)
        , ("intensity", showJSON $ mIntensity x) ]
    readJSON (JSObject obj) = Light <$> f "position" <*> f "intensity"
        where f x = mLookup x (fromJSObject obj) >>= readJSON

instance JSON Material where
    showJSON x = makeObj
        [ ("diffuse", showJSON $ mDiffuse x)
        , ("reflect", showJSON $ mReflect x) ]
    readJSON (JSObject obj) = Material <$> f "diffuse" <*> f "reflect"
        where f x = mLookup x (fromJSObject obj) >>= readJSON
