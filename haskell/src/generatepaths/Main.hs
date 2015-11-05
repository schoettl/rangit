{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docopt|usage:
  generatepaths [options] <foldername>

options:
  -x, --x-step=<deltax>  take given x step in path calculation [default: 1].
|]

getArgOrExit = getArgOrExitWith patterns

functions =
    [ ("square",       (^2)                      )
    , ("cube",         \ x -> (x*0.2)^3          )
    , ("squareroute",  sqrt                      )
    , ("squareoffs",   \ x -> 101 - (10 - x)^2   )
    , ("sine",         \ x -> 80 * sin (x*pi/50) )
    , ("cosine",       \ x -> 80 * cos (x*pi/50) )
    , ("exponential",  \ x -> exp (x*0.05)       )
    ]

maxValue = 100

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    let Just foldername = getArg args (argument "foldername")
        Just xStepStr = getArg args (longOption "x-step")
        xStep = read xStepStr :: Double

    mapM_ (savePath xStep foldername) functions

savePath :: Double -> FilePath -> (String, Double -> Double) -> IO ()
savePath xStep foldername (s, f) = do
    let xs = [0, xStep ..maxValue]
        ys = map f xs
        ys' = takeWhile (<=maxValue) ys
        pairs = zip xs ys'
        lines = map (unwords . map show . (\ (x, y) -> [x, y])) pairs
    writeFile (foldername ++ "/path_" ++ s ++ ".txt") $ unlines lines
