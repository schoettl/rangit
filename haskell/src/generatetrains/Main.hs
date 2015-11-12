{-# LANGUAGE QuasiQuotes #-}

module Main where

import Rangit.Train
import Rangit.IO
import System.Environment (getArgs)
import System.Console.Docopt
import Control.Arrow
import Data.Aeson


patterns :: Docopt
patterns = [docopt|usage:
  generatetrains [options] <foldername>

options:
  --format=<format>  output format; possible values: json, haskell
                     [default: haskell]
|]

getArgOrExit = getArgOrExitWith patterns

-- train configuration

powerCar = Part origin 0 0 3
trailer1 = Part origin 0 0 1
trailer2 = Part origin 0 0 3

powerCarRearSteer = Part origin 0 (-3) 3

powerCarTrain = [powerCar]
twoAxesTrailer = [trailer2, trailer1]
powerCarRearSteerTrain = [powerCarRearSteer]

-- | Trains to be saved. First value in the tuple is the one-word description.
-- Please one word only because it is also used as a part of the result
-- filename.
trains =
    [ ("car"              , powerCarTrain)
    , ("trailer_car"      , trailer2 : powerCarTrain)
    , ("2axestrailer_car" , twoAxesTrailer ++ powerCarTrain)
    , ("carrearsteer"     , powerCarRearSteerTrain)
    ]

-- main

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    let Just foldername = getArg args (argument "foldername")
    let Just format = getArg args (longOption "format")
    let trainsFixed = map (second fixInitialPositions) trains
    mapM_ (saveTrain format foldername) trainsFixed


saveTrain
    :: String
    -> FilePath
    -> (String, Train)
    -> IO ()
saveTrain format foldername (s, t) =
    let formatFunction :: Train -> String
        formatFunction = case format of
                           "haskell" -> show
                           "json"    -> encodeTrainAsJson
                           _         -> error "illegal format specifier"
     in writeFile (foldername ++ "/" ++ s ++ "." ++ format) $ formatFunction t
