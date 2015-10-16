module Main where

import Rangit.Train

main :: IO ()
main = interact program

program :: String -> String
program input =
    let trains = map (read :: String -> Train) $ lines input
        hitches = map (\ train -> calculateLeftHitchPosition (head train) : map partPosition train) trains
        axes = map (map calculateCenterPosition) trains
    in unlines $ zipWith (\ a b -> unwords $ positionsToStrings a ++ positionsToStrings b) hitches axes

positionsToStrings :: [Position] -> [String]
positionsToStrings ps = show (length ps) : map show (concatMap positionToList ps)

positionToList :: Position -> [Double]
positionToList (Position x y) = [x, y]
