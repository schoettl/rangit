module Rangit.AISpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.AI
import Data.Angle
import Control.Exception (evaluate)

spec :: Spec
spec = do

    describe "euclidianDistance" $ do
        it "works correct for a simple test case" $ do
            euclidianDistance (Position 0 0) (Position 1 1) `shouldAlmostBe` sqrt 2

    describe "calculateAngleInPath" $ do
        context "illegal path argument" $ do
            it "throws error for empty path" $ do
                evaluate (calculateAngleInPath [] 0) `shouldThrow` anyErrorCall
            it "throws error for path with only 1 point" $ do
                evaluate (calculateAngleInPath [Position 0 0] 0) `shouldThrow` anyErrorCall
        context "legal path argument" $ do
            context "path with 2 points" $ do
                let path = [Position 0 0, Position 1 0]
                it "works for distance -1" $ do
                    calculateAngleInPath path (-1) `shouldBe` 0
                it "works for distance 0" $ do
                    calculateAngleInPath path 0    `shouldBe` 0
                it "works for distance 0.5" $ do
                    calculateAngleInPath path 0.5  `shouldBe` 0
                it "works for distance 1" $ do
                    calculateAngleInPath path 1    `shouldBe` 0
                it "works for distance 2" $ do
                    calculateAngleInPath path 2    `shouldBe` 0
            context "path with 3 points" $ do
                let path = [Position 0 0, Position 1 0, Position 1 1]
                it "works for distance -1" $ do
                    calculateAngleInPath path (-1) `shouldBe` 0
                it "works for distance 0" $ do
                    calculateAngleInPath path 0    `shouldBe` 0
                it "works for distance 0.5" $ do
                    calculateAngleInPath path 0.5  `shouldBe` 0
                it "works for distance 1" $ do
                    calculateAngleInPath path 1    `shouldBe` (pi/2)
                it "works for distance 1.5" $ do
                    calculateAngleInPath path 1.5  `shouldBe` (pi/2)
                it "works for distance 2" $ do
                    calculateAngleInPath path 2    `shouldBe` (pi/2)
                it "works for distance 3" $ do
                    calculateAngleInPath path 3    `shouldBe` (pi/2)

    describe "backupTrainToFitPath" $ do
        let path = [origin, Position (-1) 0]
            pc = Part origin 0 1 1
        return ()

    let secondFromEnd = last . init
        path = [origin, Position (-1) 0, Position (-2) (-1)]
        train = fixInitialPositions [Part origin 0 0 1, Part (Position 2 0) 0 1 1]

    describe "backupTrain" $ do
        let newTrain = backupTrain path train
            target = secondFromEnd path
        return ()

    describe "backupTrainAccumulateDriveCommands" $ do
        let result = backupTrainAccumulateDriveCommands path train
        it "should return n drive commands where n = len path - 1" $ do
            length result `shouldBe` length path - 1
        --it "approaches the target i.e. second point of path from the end" $ do
        --    isFallingSeries $ map (euclidianDistance (secondFromEnd path) . trainPosition . snd) result
        --where
        --    isFallingSeries = foldl f (True, Nothing)
        --        where
        --            f (_, Nothing) val = (True, Just val)
        --            f (True, Just last) val
        --                | val < last = (True, Just val)
        --                | otherwise  = (False, Just last)
        --            f a@(False, _) _ = a
