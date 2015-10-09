{-# LANGUAGE QuasiQuotes #-}

module Main where

import Rangit.Train

-- train configuration

myPowerCar = Part origin 0 1 2
myTrailer1 = Part origin (pi/2) 0 2
myTrailer2 = Part origin (pi/3) 0 3

myTrain = fixInitialPositions $ myTrailer2 : myTrailer1 : [myPowerCar]


-- main

-- | Just print the train configured in this program.
main :: IO ()
main = print myTrain
