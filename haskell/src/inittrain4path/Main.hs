module Main where

import Rangit.Train
import Rangit.Drive
import Rangit.AI
import Rangit.IO
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let pathFile = head args
    path <- loadPathFromFile pathFile
    interact (program path)

program :: DiscretePath -> String -> String
program path input =
    let train = read input
    in unlines . pure . show $ positionTrain path train
    -- unlines in interact function for trailing newline!

positionTrain :: DiscretePath -> Train -> Train
positionTrain path = translateTrainTo (first path) . fixInitialPositions . alignTrainAngleToFirstPathSegment path . setTrainPositionToFirstPathPoint path

alignTrainAngleToFirstPathSegment :: DiscretePath -> Train -> Train
alignTrainAngleToFirstPathSegment (a:b:_) =
    let angle = calculateAngleBetweenPoints b a
    in map (\ p -> p { partAngle = angle })

setTrainPositionToFirstPathPoint :: DiscretePath -> Train -> Train
setTrainPositionToFirstPathPoint (p:_) train = init train ++ [(last train) { partPosition = p }]
