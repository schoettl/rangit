{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO
import Data.Angle
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Rangit.Train
import Rangit.Drive

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

data Command = Command Float Float

-- user code --

myCar = Part origin 0 1 5
myTrailer = Part origin 0 0 4

myTrain = myTrailer : [myCar]

-- | Drive train reading from standard input and
-- writing new positions to standard output.
--
-- 1. Read lines of input (each length and steer angle),
-- 2. drive the train according to the input,
-- 3. output new position as JSON.
main :: IO ()
main = do
    --hSetBuffering stdin NoBuffering
    putStrLn $ formatOutput myTrain
    interact program

program :: String -> String
program input = unlines $
    map (formatOutput . executeCommand myTrain) $
        (convertToCommands . map words . lines) input

convertToCommands :: [[String]] -> [Command]
convertToCommands = map (\ (x:a:_) -> Command (read x :: Float) (read a :: Float))

executeCommand :: [Part] -> Command -> [Part]
executeCommand ps (Command x a) = drive ps x (degreesToRadians a)

formatOutput :: [Part] -> String
--formatOutput = encodeAsJson
formatOutput = show

encodeAsJson :: [Part] -> String
encodeAsJson = BSL.unpack . encode . toJSON

degreesToRadians :: Float -> Float
degreesToRadians a = let Radians x = radians $ Degrees a in x
