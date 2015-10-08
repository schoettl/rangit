{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO
import Data.Angle
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Rangit.Train
import Rangit.Drive
import System.Console.Docopt

{- | Later module description
 - All length and position measures are in meter.
 - All angle measures are in rad, going counter-clockwise from the West-East (horizontal) line to the part.
 - The power car is always right-most.
 - There can be an abitrary number of parts at the left side of the power car.
 -}

instance ToJSON Position where
    toJSON (Position x y) = object [ "x" .= x , "y" .= y ]

instance ToJSON Part where
    toJSON (Part position angle leftLength rightLength) =
        object [ "position"    .= position
               , "angle"       .= angle
               , "leftLength"  .= leftLength
               , "rightLength" .= rightLength
               ]

-- user code --

myCar = Part origin 0 1 5
myTrailer = Part origin 1 0 4

myTrain = fixInitialPositions $ myTrailer : [myCar]

-- | Drive train reading from standard input and
-- writing new positions to standard output.
--
-- 1. Read lines of input (each length and steer angle),
-- 2. drive the train according to the input,
-- 3. output new position as JSON.
main :: IO ()
main = interact program

program :: String -> String
program input = unlines $
    map formatOutput $
        processCommands myTrain $
            (convertToCommands . map words . lines) input

processCommands :: Train -> [DriveCommand] -> [Train]
processCommands = scanl executeCommand

convertToCommands :: [[String]] -> [DriveCommand]
convertToCommands = map (\ (x:a:_) -> DriveCommand (read x :: Double) (read a :: Double))

executeCommand :: Train -> DriveCommand -> Train
executeCommand ps (DriveCommand x a) = drive ps x (degreesToRadians a)

formatOutput :: Train -> String
--formatOutput = encodeAsJson
formatOutput = show

encodeAsJson :: Train -> String
encodeAsJson = BSL.unpack . encode . toJSON

degreesToRadians :: Double -> Double
degreesToRadians a = let Radians x = radians $ Degrees a in x
