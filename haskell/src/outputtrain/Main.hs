module Main where

import Rangit.Train

-- train configuration

myPowerCar = Part origin 0 0 4
myTrailer1 = Part origin (pi/2) 0 2
myTrailer2 = Part origin (pi/3) 0 3

myTrain = fixInitialPositions $ [myPowerCar]


-- main

-- | Just print the train configured in this program.
main :: IO ()
main = print myTrain
