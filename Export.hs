-- Copyright 2012 Mitchell Kember.

module Export (export) where

import Data.Monoid ((<>), mconcat)
import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder
    ( Builder, fromWord8s, fromWrite, toLazyByteString
    , writeWord16le, writeWord8 )

import Colour (Colour24, VectorT(..))
import Trace (Settings, mResolutionX, mResolutionY)

-- Convert a 24-bit colour to a builder in the channel order of RGB.
fromColourRGB :: Colour24 -> Builder
fromColourRGB (Vector r g b) = fromWrite $
    writeWord8 r <> writeWord8 g <> writeWord8 b

-- Convert a 24-bit colour to a builder in the channel order of BGR.
fromColourBGR :: Colour24 -> Builder
fromColourBGR (Vector r g b) = fromWrite $
    writeWord8 b <> writeWord8 g <> writeWord8 r

-- Create the header for a TGA image using the resolution found in the settings.
tgaHeader :: Settings -> Builder
tgaHeader settings
    = (fromWord8s [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0] <>) $ fromWrite
    $  writeWord16le (fromIntegral (mResolutionX settings))
    <> writeWord16le (fromIntegral (mResolutionY settings))
    <> writeWord8 24 <> writeWord8 32

-- Export a rendered scene to a TGA file as a lazy ByteString.
export :: Settings -> [Colour24] -> L.ByteString
export settings cs = toLazyByteString $
    tgaHeader settings <> mconcat (map fromColourBGR cs)
