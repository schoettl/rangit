module Main where

functions =
    [ (^2)
    , (^3)
    , sqrt
    , \ x -> 80 * sin x
    , \ x -> 80 * cos x
    , \ x -> exp x
    ]

maxValue = 100

xs = [0..maxValue]

main :: IO ()
main = mapM_ savePath $ zip [1..] functions

savePath :: (Int, (Double -> Double)) -> IO ()
savePath (i, f) = do
    let ys = map f xs
        ys' = takeWhile (<=maxValue) ys
        pairs = zip xs ys'
        lines = map (unwords . map show . (\ (x, y) -> [x, y])) pairs
    writeFile ("paths/path" ++ show i ++ ".mat") $ unlines lines
