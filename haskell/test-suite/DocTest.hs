module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)
import Control.Applicative

paths = ["src"]

main :: IO ()
main = sequence (( \p -> glob $ p ++ "/**/*.hs") <$> paths) >>= doctest . concat
