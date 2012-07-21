-- Copyright 2012 Mitchell Kember

-- | The main module.
module Main (main) where

import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr)
import Text.JSON (Result(..), decode)
import qualified Data.ByteString.Lazy as L

import Luminosity.Image.Export (export, extension)
import Luminosity.Render (render)
import Luminosity.Text.Parse ()
import Luminosity.Trace (Scene, mResolutionX, mResolutionY, mSettings)

-- | The current version string.
version :: String
version = "1.0"

-- | The help message: general information such as the version and copyright,
-- and instructions on how to use the program (the usage message).
help :: String
help = "Luminosity version " ++ version
    ++ " - Copyright 2012 Mitchell Kember\n"
    ++ "Usage: luminosity input_file[.json] [output_file[.tga]]"

-- | @withFiles input output@ parses @input@ as a JSON representation of a
-- 'Scene', renders it and exports it, and writes the image to @output@.
withFiles :: FilePath -> FilePath -> IO ()
withFiles input output = readFile input >>= \file -> case decode file of
    Ok scene -> let
        width  = mResolutionX $ mSettings scene
        height = mResolutionY $ mSettings scene
        in L.writeFile output $ export width height $ render scene
    Error s  -> putStrLn s

-- | The main function.
main :: IO ()
main = getArgs >>= \args -> case args of
    ["-h"]     -> putStrLn help
    ["--help"] -> putStrLn help
    [x, y]     -> withFiles x y
    [x]        -> withFiles x $ replaceExtension x extension
    _          -> hPutStrLn stderr help
