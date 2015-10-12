module Main where

functions =
    [ ("square",       (^2)              )
    , ("cube",         (^3)              )
    , ("squareroute",  sqrt              )
    , ("sine",         \ x -> 80 * sin x )
    , ("cosine",       \ x -> 80 * cos x )
    , ("exponential",  \ x -> exp x      )
    ]

maxValue = 100

xs = [0..maxValue]

main :: IO ()
main = mapM_ savePath $ zip [1..] functions

savePath :: (Int, (String, Double -> Double)) -> IO ()
savePath (i, (s, f)) = do
    let ys = map f xs
        ys' = takeWhile (<=maxValue) ys
        pairs = zip xs ys'
        lines = map (unwords . map show . (\ (x, y) -> [x, y])) pairs
    writeFile ("paths/path_" ++ s ++ ".mat") $ unlines lines
