{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.IO
import Data.Angle
import Rangit.Train
import Rangit.Drive
import Rangit.IO
import Data.Maybe
import System.Environment (getArgs)
import System.Console.Docopt

{- | Later module description
 - All length and position measures are in meter.
 - All angle measures are in rad, going counter-clockwise from the West-East (horizontal) line to the part.
 - The power car is always right-most.
 - There can be an abitrary number of parts at the left side of the power car.
 -}

patterns :: Docopt
patterns = [docopt|usage: simulation [options] <trainfile>

options:
  -i, --print-interval=<meters>
        Print train every x meters of driving. Set x to 0 to print train only
        after each drive command. [default: 0.1]
|]

getArgOrExit = getArgOrExitWith patterns

-- | Drive train reading from standard input and
-- writing new positions to standard output.
--
-- 1. Read lines of input (each length and steer angle),
-- 2. drive the train according to the input,
-- 3. output new position as Haskell object or JSON.
main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    train <- readTrain . fromJust . getArg args $ argument "trainfile"
    let interval = read . fromJust . getArg args $ longOption "print-interval" :: Double
        filterResult = if interval <= 0
            then id
            else selectTrainsToPrint interval
    interact (program filterResult train)

program :: ([Train] -> [Train]) -> Train -> String -> String
program filterResult train input =
    unlines . map formatOutput .
        filterResult . processCommands train .
            convertToCommands . map words . lines $ input

processCommands :: Train -> [DriveCommand] -> [Train]
processCommands = scanl executeCommand

convertToCommands :: [[String]] -> [DriveCommand]
convertToCommands = map (\ (x:a:_) -> DriveCommand (read x :: Double) (read a :: Double))

executeCommand :: Train -> DriveCommand -> Train
executeCommand ps (DriveCommand x a) = drive ps x (degreesToRadians a)

formatOutput :: Train -> String
--formatOutput = encodeTrainAsJson
formatOutput = show

degreesToRadians :: Double -> Double
degreesToRadians a = let Radians x = radians $ Degrees a in x

readTrain :: FilePath -> IO Train
readTrain s = do
    contents <- readFile s
    return $ read contents

-- | In the list of moved trains keep every n-th train with n so that there is
-- the given way distance between the trains.
selectTrainsToPrint :: Double -> [Train] -> [Train]
selectTrainsToPrint interval trains = let modulus = calculateModulus interval
    in snd . unzip . filter (\ (n, _) -> n `mod` modulus == 0) . zip [0..] $ trains

-- | Calculate modulus that is used in selectTrainsToPrint
calculateModulus :: Double -> Int
calculateModulus interval
    | interval <= 0          = error "this function only handles values > 0"
    | interval <= stepLength = ceiling quotient
    | interval == stepLength = 1
    | otherwise              = round quotient
    where quotient = interval / stepLength
