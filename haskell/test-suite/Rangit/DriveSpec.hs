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
            it "calculates new position correctly" $ do
                let Position x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (1-stepLength)) && x < (1+stepLength)
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving a car to the left (negative distance)" $ do
            let movedTrain = drive train (-1) 0
                [movedCar] = movedTrain
            it "calculates new position correctly" $ do
                let Position x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (-1-stepLength)) && x < (-1+stepLength)
                y `shouldBe` 0
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldBe` 0
        context "driving car with trailer" $ do
            let trailer = Part origin 0 undefined 1
                trainWithTrailer = fixInitialPositions $ trailer:train
                movedTrain = drive trainWithTrailer (-1) 0
                movedTrailer:_ = movedTrain
            it "calculates new trailer position" $ do
                let Position x y = partPosition movedTrailer
                x `shouldSatisfy` \x -> (x > (-2-stepLength)) && x < (-2+stepLength)
                y `shouldBe` 0
            it "calculates new angle correctly" $ do
                partAngle movedTrailer `shouldBe` 0
        context "bringing car from <90° to >90°" $ do
            let train = [Part origin (pi/2-0.1) 0 1]
                movedTrain = drive train 1 (pi/4)
                [movedCar] = movedTrain
            it "calculates new position plausible" $ do
                let Position x y = partPosition movedCar
                x `shouldSatisfy` (<0)
                y `shouldSatisfy` (>0)
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldSatisfy` (>pi/2)

    describe "movePart" $ do
        context "move one part" $ do
            let powerCar = Part origin 0 0 1
                targetPosition = Position 1 1
                newAngle = atan $ 1 / (1 + 1) -- 1 from dimensions of power car
                ([pc], _) = movePart powerCar ([], targetPosition)
            it "updates the position" $ do
                partPosition pc `shouldBe` targetPosition
            it "updates the angle" $ do
                partAngle pc @=~? newAngle
        context "move part from <90° to >90°" $ do
            let part = Part origin (pi/2) 0 1
                targetPosition = Position (-1) 1
                ([movedPart], _) = movePart part ([], targetPosition)
            it "updates position right" $ do
                partPosition movedPart `shouldBe` targetPosition
            it "updates angle right" $ do
                partAngle movedPart `shouldSatisfy` (>pi/2)
                partAngle movedPart `shouldSatisfy` (<pi)

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
        it "works for x and y equals 0" $ do
            calculateAngleByArcTan 0 0 `shouldBe` 0
        it "angles below the horizontal line are negative" $ property $
            \ x -> calculateAngleByArcTan x (-1) < 0
        it "angles above the horizontal line are positive" $ property $
            \ x -> calculateAngleByArcTan x 1 > 0
        it "angles mirrowed at the horizontal line differ olny in sign" $ property $
            \ x -> calculateAngleByArcTan x 1 == -calculateAngleByArcTan x (-1)
