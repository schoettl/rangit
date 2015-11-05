module Rangit.AISpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.AI
import Data.Angle
import Control.Exception (evaluate)
import Data.Vector.Extended (Vector2 (Vector2), euclidianDistance)

spec :: Spec
spec = do

    describe "euclidianDistance" $ do
        it "works correct for a simple test case" $ do
            euclidianDistance (Vector2 0 0) (Vector2 1 1) `shouldAlmostBe` sqrt 2

    describe "calculateAngleInPath" $ do
        context "illegal path argument" $ do
            it "throws error for empty path" $ do
                evaluate (calculateAngleInPath [] 0) `shouldThrow` anyErrorCall
            it "throws error for path with only 1 point" $ do
                evaluate (calculateAngleInPath [Vector2 0 0] 0) `shouldThrow` anyErrorCall
        context "legal path argument" $ do
            context "path with 2 points" $ do
                let path = [Vector2 0 0, Vector2 1 0]
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
                let path = [Vector2 0 0, Vector2 1 0, Vector2 1 1]
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

    describe "backupTrainAccumulateDriveCommands" $ do
        let train = fixInitialPositions [Part origin pi 0 1, Part origin pi 1 1]
            path = [Vector2 5 0, Vector2 6 0, Vector2 7 (-1)]
            result = backupTrainAccumulateDriveCommands path train
        it "should return as many drive commands as there are points in path,\
           \as long as no waypoints are overrunned" $ do
            length result `shouldBe` length path
