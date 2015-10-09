{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Console.Docopt
import Vision.Image
import Vision.Image.Storage.DevIL
import Data.Char
import Data.Maybe

patterns :: Docopt
patterns = [docopt|
image2path version 1.0

usage:
  image2path [options] <image_file>

options:
  -f, --flip            flip image vertically before extracting path
  -x, --x-offset=<val>  offset to add to the x coordinates of the path [default: 0]
  -y, --y-offset=<val>  offset to add to the y coordinates of the path [default: 0]
  -v, --invert          invert path direction
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    let Just filename = getArg args (argument "image_file")
    io <- load Autodetect filename
    case io of
        Left err -> do
            putStrLn "Unable to load the image:"
            print err
        Right (rgb :: RGB) -> do
            let image = if isPresent args (longOption "flip")
                    then verticalFlip rgb
                    else rgb
            print rgb
