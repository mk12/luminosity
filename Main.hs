-- Copyright 2012 Mitchell Kember.

module Main (main) where

import Control.Monad ((=<<))
import System.Environment (getArgs, getProgName)
import System.FilePath (FilePath, replaceExtension)
import System.IO (hPutStrLn, stderr)
import Text.JSON (Result(..), decode)
import qualified Data.ByteString.Lazy as L

import Export (export)
import Parse
import Render (render)
import Trace (Scene, mSettings)

-- The current version string.
version :: String
version = "1.0"

-- The usage message: general information such as the version and copyright, and
-- instructions on how to use the program.
usage :: IO String
usage = do
    name <- getProgName
    return $ "Luminosity version " ++ version ++ "    "
        ++ "Copyright 2012 Mitchell Kember.\n"
        ++ "Usage: " ++ name ++ " input_file[.json] [output_file[.tga]]"

-- Given an input file path and an ouput file path, parse the input file, render
-- the scene, export it to the output format, and write it to the output file.
files :: FilePath -> FilePath -> IO ()
files input output = do
    f <- readFile (replaceExtension input "json")
    case decode f of
        Ok scene -> L.writeFile (replaceExtension output "tga") $
            export (mSettings scene) (render scene)
        Error s  -> putStrLn s

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"]     -> putStrLn =<< usage
        ["--help"] -> putStrLn =<< usage
        [x, y]     -> files x y
        [x]        -> files x x
        _          -> hPutStrLn stderr =<< usage
