{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Rangit.IO where

import Rangit.Train
import Rangit.AI
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

instance ToJSON Position where
    toJSON (Position x y) = object [ "x" .= x , "y" .= y ]

instance ToJSON Part where
    toJSON (Part position angle leftLength rightLength) =
        object [ "position"    .= position
               , "angle"       .= angle
               , "leftLength"  .= leftLength
               , "rightLength" .= rightLength
               ]

encodeTrainAsJson :: Train -> String
encodeTrainAsJson = BSL.unpack . encode . toJSON

loadTrainFromFile :: FilePath -> IO Train
loadTrainFromFile = doReadFile loadTrain

loadTrain :: Handle -> IO Train
loadTrain h = do
    contents <- hGetContents h
    return $ read contents


loadPathFromFile :: FilePath -> IO DiscretePath
loadPathFromFile = doReadFile loadPath

loadPath :: Handle -> IO DiscretePath
loadPath h = do
    contents <- hGetContents h
    let allLines = lines contents
        positionLines = filter (\ (c:_) -> c /= '#') allLines -- ignore comment lines
        path = map (toPosition . map read . words) positionLines
    return path

doReadFile :: (Handle -> IO a) -> FilePath -> IO a
doReadFile f s = f =<< openFile s ReadMode

toPosition :: [Double] -> Position
toPosition (x:y:_) = Position x y
