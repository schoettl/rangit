module Main where

import Rangit.Train
import Rangit.AI
import Rangit.IO
import System.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let pathFile = head args
    path <- loadPathFromFile pathFile
    interact (positionTrain path)

positionTrain :: DiscretePath -> String -> String
positionTrain path input =
    let train = read input
    in unlines . pure . show $ calculateIdealTrain path train
    -- unlines in interact function for trailing newline!
