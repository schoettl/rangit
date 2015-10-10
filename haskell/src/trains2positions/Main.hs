module Main where

import Rangit.Train

main :: IO ()
main = interact program

program :: String -> String
program input =
    let trains = map (read :: String -> Train) $ lines input
        hitches = map (\ train -> map partPosition train ++ [calculateLeftHitchPosition (last train)]) trains
        axis = map (\ train -> map calculateCenterPosition train) trains
    in unlines $ zipWith (\ a b -> unwords $ positionsToStrings a ++ positionsToStrings b) hitches axis

positionsToStrings :: [Position] -> [String]
positionsToStrings ps = show (length ps) : map show (concat $ map positionToList ps)

positionToList :: Position -> [Double]
positionToList (Position x y) = [x, y]
