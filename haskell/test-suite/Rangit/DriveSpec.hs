module Rangit.DriveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.Drive

spec :: Spec
spec = do

    describe "drive" $ do
        let car = Part origin 0 0 1
            train = [car]
        context "driving a car to the right (positive distance)" $ do
            let movedTrain = drive train 1 0
                [movedCar] = movedTrain
                Position x y = partPosition movedCar
            it "calculates new position correctly" $ do
                x `shouldSatisfy` \x -> (x > (1-stepLength)) && x < (1+stepLength)
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving a car to the left (negative distance)" $ do
            let movedTrain = drive train (-1) 0
                [movedCar] = movedTrain
                Position x y = partPosition movedCar
            it "calculates new position correctly" $ do
                x `shouldSatisfy` \x -> (x > (-1-stepLength)) && x < (-1+stepLength)
                y `shouldBe` 0
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving car with trailer" $ do
            let trailer = Part origin 0 undefined 1
                trainWithTrailer = fixInitialPositions $ trailer:train
                movedTrain = drive trainWithTrailer (-1) 0
                movedTrailer:_ = movedTrain
                Position x y = partPosition movedTrailer
            it "calculates new trailer position" $ do
                x `shouldSatisfy` \x -> (x > (-2-stepLength)) && x < (-2+stepLength)
                y `shouldBe` 0
            it "calculates new angle" $ do
                partAngle movedTrailer `shouldBe` 0


    describe "movePart" $ do
        context "move one part" $ do
            let powerCar = Part origin 0 0 1
                targetPosition = Position 1 1
                newAngle = atan $ 1 / (1 + 1) -- 1 from dimensions of power car
                ([pc], lh) = movePart powerCar ([], targetPosition)
            it "updates the position" $ do
                partPosition pc `shouldBe` targetPosition
            it "updates the angle" $ do
                partAngle pc @=~? newAngle

    describe "calculateAngleByArcTan" $ do
        --it "works for an angle less than 45°" $ do
        --    calculateAngleByArcTan 2 1 `shouldBe` ...
        it "works for 0°" $ do
            calculateAngleByArcTan 1 0 `shouldBe` 0
        it "works for 90°" $ do
            calculateAngleByArcTan 0 1 `shouldBe` (pi/2)
        it "works for 180°" $ do
            calculateAngleByArcTan (-1) 0 `shouldBe` pi
        it "works for -90°" $ do
            calculateAngleByArcTan 0 (-1) `shouldBe` (-pi/2)
