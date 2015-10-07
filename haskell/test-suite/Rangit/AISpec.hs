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

    describe "weightedAngleDiffs" $ do
        it "weights angle differences with even exponents" $ do
            weightAngleDiffs [4, 3, 2] `shouldBe` [16, 6, 2]

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

    describe "calculateIdealTrain" $ do
        let path = [Position 0 0, Position 2 2, Position 3 2]
            initPosition = Position (-1) (-1)
            powerCar = Part initPosition 0 1 1
        context "just the power car" $ do
            let [powerCar'] = calculateIdealTrain path [powerCar]
            it "has correct position" $ do
                partPosition powerCar' `shouldBe` Position 0 0
            it "has correct angle" $ do
                partAngle powerCar' `shouldBe` -3/4*pi
        context "power car and trailer" $ do
            let trailer = Part initPosition 0 1 1
                [tr, pc] = calculateIdealTrain path [trailer, powerCar]
            it "power car has correct position" $ do
                partPosition pc `shouldBe` Position 0 0
            it "power car has correct angle" $ do
                partAngle pc `shouldBe` -3/4*pi
            it "trailer has correct position" $ do
                partPosition tr `shouldAlmostBe` Position (sqrt 2) (sqrt 2)
            it "trailer has correct angle" $ do
                -- -pi oder +pi, hauptsache 180° verdreht halt
                partAngle tr `shouldBe` -pi
