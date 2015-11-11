{-# LANGUAGE QuasiQuotes #-}

module Main where

import Rangit.Train
import System.Environment (getArgs)
import System.Console.Docopt
import Control.Arrow


patterns :: Docopt
patterns = [docopt|usage:
  generatetrains <foldername>
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
    let trainsFixed = map (second fixInitialPositions) trains
    mapM_ (saveTrain foldername) trainsFixed


saveTrain :: FilePath -> (String, Train) -> IO ()
saveTrain foldername (s, t) = writeFile (foldername ++ "/" ++ s ++ ".dat") $ show t
