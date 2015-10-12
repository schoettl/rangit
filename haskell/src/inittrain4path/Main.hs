module Main where

import Rangit.Train
import Rangit.AI
import Rangit.IO
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let pathFile = args !! 0
    path <- loadPathFromFile pathFile
    interact (positionTrain path)

positionTrain :: DiscretePath -> String -> String
positionTrain path input =
    let train = read input
    in show $ calculateIdealTrain path train
