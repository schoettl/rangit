{-# LANGUAGE QuasiQuotes #-}

module Main where

import Rangit.Train
import Rangit.Drive
import Rangit.AI
import Rangit.IO
import Data.Maybe
import Data.Angle
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docopt|backupai version 1.0

usage:
  backupai [options] <pathfile> <trainfile>

options:
|]

getArgOrExit = getArgOrExitWith patterns

-- | Output drive commands for a given train to drive a given route.
main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    path  <- loadPathFromFile  $ getPositionalArg args "pathfile"
    train <- loadTrainFromFile $ getPositionalArg args "trainfile"
    let commands = backupTrainAccumulateDriveCommands path train
    mapM_ printDriveCommand commands

getPositionalArg :: Arguments -> String -> String
getPositionalArg args name = fromJust $ getArg args (argument name)

printDriveCommand :: DriveCommand -> IO ()
printDriveCommand (DriveCommand l a) = putStrLn $ show l ++ " " ++ show (radiansToDegree a)

radiansToDegree :: Double -> Double
radiansToDegree a = let Degrees x = degrees $ Radians a in x
