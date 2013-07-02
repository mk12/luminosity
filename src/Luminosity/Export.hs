-- Copyright 2012 Mitchell Kember. Subject to the MIT License.

-- | A simple TGA image exporter.
module Luminosity.Export (extension, export) where

import Data.Monoid ((<>), mconcat)
import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder
    ( Builder, fromWord8s, fromWrite, toLazyByteString
    , writeWord16le, writeWord8 )

import Luminosity.Colour (Colour24, VectorT(..))

-- TODO: add image dimensions/data type
-- data Image = Int Int [Colour24]

-- | The file extension used for the image output format.
extension :: String
extension = "tga"

-- | Convert a 24-bit colour to a builder in the channel order of BGR.
fromColourBGR :: Colour24 -> Builder
fromColourBGR (Vector r g b) = fromWrite $
    writeWord8 b <> writeWord8 g <> writeWord8 r

-- | @tgaHeader width height@ creates the binary header for a TGA image using
-- the given resolution.
tgaHeader :: Int -> Int -> Builder
tgaHeader width height
    = (fromWord8s [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0] <>) $ fromWrite
    $  writeWord16le (fromIntegral width)
    <> writeWord16le (fromIntegral height)
    <> writeWord8 24 <> writeWord8 32

-- | @export width height cs@ exports @cs@ to a TGA image as a lazy
-- @ByteString@.
export :: Int -> Int -> [Colour24] -> L.ByteString
export width height cs = toLazyByteString $
    tgaHeader width height <> mconcat (map fromColourBGR cs)
