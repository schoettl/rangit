{-# LANGUAGE QuasiQuotes #-}

module Main where

import Rangit.Train
import Rangit.Drive
import Rangit.AI
import Data.Maybe
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docopt|
backupai version 1.0

usage:
  backupai [options] <pathfile> <trainfile>

options:
|]

getArgOrExit = getArgOrExitWith patterns

-- | Output drive commands for a given train to drive a given route.
main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    path  <- readPath  $ getPositionalArg args "pathfile"
    train <- readTrain $ getPositionalArg args "trainfile"
    let commands = backupTrainAccumulateDriveCommands path train
    mapM_ printDriveCommand commands

readPath :: FilePath -> IO DiscretePath
readPath s = do
    contents <- readFile s
    return $ map (toPosition . map read . words) $ lines contents

readTrain :: FilePath -> IO Train
readTrain s = do
    contents <- readFile s
    return $ read contents

toPosition :: [Double] -> Position
toPosition (x:y:_) = Position x y

getPositionalArg :: Arguments -> String -> String
getPositionalArg args name = fromJust $ getArg args (argument name)

printDriveCommand :: DriveCommand -> IO ()
printDriveCommand (DriveCommand l a) = putStrLn $ show l ++ " " ++ show a
