module Main where

import Rangit.Train
import Rangit.Drive

{- | Later module description
 - All length and position measures are in meter.
 - All angle measures are in rad, going counter-clockwise from the West-East (horizontal) line to the part.
 - The power car is always right-most.
 - There can be an abitrary number of parts at the left side of the power car.
 -}



-- user code --

myCar = Part origin 0 1 5
myTrailer = Part origin 0 0 4

myTrain = myTrailer : [myCar]

main = return ()
