module Rangit.DriveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.Drive
import Rangit.Math
import Data.Vector.Extended (Vector2 (Vector2), euclidianDistance)

spec :: Spec
spec = do

    describe "drive" $ do
        let car = Part origin 0 0 1
            train = [car]
        context "driving a car to the right (positive distance)" $ do
            let movedTrain = drive train 1 0
                [movedCar] = movedTrain
            it "calculates new position correctly" $ do
                let Vector2 x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (1-stepLength)) && x < (1+stepLength)
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving a car to the left (negative distance)" $ do
            let movedTrain = drive train (-1) 0
                [movedCar] = movedTrain
            it "calculates new position correctly" $ do
                let Vector2 x y = partPosition movedCar
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
                let Vector2 x y = partPosition movedTrailer
                x `shouldSatisfy` \x -> (x > (-2-stepLength)) && x < (-2+stepLength)
                y `shouldBe` 0
            it "calculates new angle correctly" $ do
                partAngle movedTrailer `shouldBe` 0
        context "bringing car from <90° to >90°" $ do
            let train = [Part origin (pi/2-0.1) 0 1]
                movedTrain = drive train 1 (pi/4)
                [movedCar] = movedTrain
            it "calculates new position plausible" $ do
                let Vector2 x y = partPosition movedCar
                x `shouldSatisfy` (<0)
                y `shouldSatisfy` (>0)
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldSatisfy` (>pi/2)

    describe "driveAccumulateTrains" $ do
        let car = Part origin 0 0 1
            train = [car]
        context "driving a car accumulating intermediate positions" $ do
            it "there are 0 trains in the list for a drive of 0 meters" $ do
                let allTrains = driveAccumulateTrains train 0 0
                allTrains `shouldBe` []
            it "there is 1 train in the list for a drive of stepLength meters" $ do
                let allTrains = driveAccumulateTrains train stepLength 0
                length allTrains `shouldBe` 1
            it "there are (1 / stepLength) trains in the list for a drive of 1 meter" $ do
                let allTrains = driveAccumulateTrains train 1 0
                length allTrains `shouldBe` ceiling (1/stepLength)
            it "there are (x / stepLength) trains in the list for diffent x" $ property $
                \ x -> length (driveAccumulateTrains train x 0) == ceiling (abs x / stepLength)
        context "driving a car to the right (positive distance)" $ do
            let allTrains = driveAccumulateTrains train 1 0
                movedTrain = last allTrains
                [movedCar] = movedTrain
            it "calculates new position correctly" $ do
                let Vector2 x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (1-stepLength)) && x < (1+stepLength)
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving a car to the left (negative distance)" $ do
            let allTrains = driveAccumulateTrains train (-1) 0
                movedTrain = last allTrains
                [movedCar] = movedTrain
            it "calculates new position correctly" $ do
                let Vector2 x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (-1-stepLength)) && x < (-1+stepLength)
                y `shouldBe` 0
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldBe` 0
        context "driving car with trailer" $ do
            let trailer = Part origin 0 undefined 1
                trainWithTrailer = fixInitialPositions $ trailer:train
                allTrains = driveAccumulateTrains trainWithTrailer (-1) 0
                movedTrain = last allTrains
                movedTrailer:_ = movedTrain
            it "calculates new trailer position" $ do
                let Vector2 x y = partPosition movedTrailer
                x `shouldSatisfy` \x -> (x > (-2-2*stepLength)) && x < (-2+2*stepLength)
                y `shouldBe` 0
            it "calculates new angle correctly" $ do
                partAngle movedTrailer `shouldBe` 0
        context "bringing car from <90° to >90°" $ do
            let train = [Part origin (pi/2-0.1) 0 1]
                allTrains = driveAccumulateTrains train 1 (pi/4)
                movedTrain = last allTrains
                [movedCar] = movedTrain
            it "calculates new position plausible" $ do
                let Vector2 x y = partPosition movedCar
                x `shouldSatisfy` (<0)
                y `shouldSatisfy` (>0)
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldSatisfy` (>pi/2)

    describe "moveTrainToPosition" $ do
        context "just a power car" $ do
            let pc = Part origin 0 0 1
                train = [pc]
            it "works for a target in front of" $ do
                let target = Vector2 1 (-2)
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = - pi/4 }]
            it "works for a target right below the axis center" $ do
                let target = Vector2 (-1) (-1)
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = - pi/2 }]
            it "works for a target behind the train" $ do
                let target = Vector2 (-2) 1
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = 3*pi/4 }]

    describe "movePart" $ do
        context "move one part" $ do
            let powerCar = Part origin 0 0 1
                targetPosition = Vector2 1 1
                newAngle = atan $ 1 / (1 + 1) -- 1 from dimensions of power car
                ([pc], _) = movePart powerCar ([], targetPosition)
            it "updates the position" $ do
                partPosition pc `shouldBe` targetPosition
            it "updates the angle" $ do
                partAngle pc `shouldAlmostBe` newAngle
        context "move part from <90° to >90°" $ do
            let part = Part origin (pi/2) 0 1
                targetPosition = Vector2 (-1) 1
                ([movedPart], _) = movePart part ([], targetPosition)
            it "updates position right" $ do
                partPosition movedPart `shouldBe` targetPosition
            it "updates angle right" $ do
                partAngle movedPart `shouldSatisfy` (>pi/2)
                partAngle movedPart `shouldSatisfy` (<pi)
        context "move two parts" $ do
            let pc = Part origin 0 0 1
                tr = Part (Vector2 (-1) 0) 0 0 1
                target1 = Vector2 1 0 -- targets for pc
                pc' = pc { partPosition = target1 }
                target2 = calculateLeftHitchPosition pc' -- target for tr
                tr' = tr { partPosition = target2 }
                target3 = calculateLeftHitchPosition tr' -- target for further parts
            it "moves first part" $ do
                movePart pc ([], target1) `shouldBe` ([pc'], target2)
            it "moves second part" $ do
                movePart tr ([pc'], target2) `shouldBe` ([tr', pc'], target3)

    describe "modReal" $ do
        context "some properties of modulo" $ do
            let modulus = 13 :: Int
            it "behaves like mod for instances of Integral type class" $ property $
                \x -> x `modReal` modulus == x `mod` modulus
            it "result is always positive (for positive modulus)" $ property $
                \x -> x `modReal` modulus >= 0
        context "modulo 2 pi" $ do
            let modulus = 2*pi
            it "2 pi modulo 2 pi" $ do
                ((2*pi) `modReal` modulus) `shouldBe` 0
            it "3 pi modulo 2 pi" $ do
                ((3*pi) `modReal` modulus) `shouldBe` pi

    let korbisPart = Part origin pi 3 6.5
        korbisTargetPosition = Vector2 11.5 (-4.25)
    describe "calculateInnerCircleRadius" $ do
        it "works for Korbinian's sketch" $ do
            let r = 4
            calculateInnerCircleRadius korbisPart korbisTargetPosition `shouldSatisfy` \ x -> x >= (r-0.01) && x <= (r+0.01)

    describe "calculateSteerAngleForBackup" $ do
        it "works for Korbinian's sketch" $ do
            let lw = pi/2 - atan (4/6.5)
            calculateSteerAngleForBackup korbisPart korbisTargetPosition `shouldSatisfy` \ x -> x >= (lw-0.01) && x <= (lw+0.01)

    describe "backupPartToPosition" $ do
        it "works for Korbinian's sketch" $ do
            let newHitchPosition = calculateLeftHitchPosition $ backupPartToPosition korbisPart korbisTargetPosition
            --newHitchPosition `shouldBe` korbisTargetPosition
            euclidianDistance newHitchPosition korbisTargetPosition `shouldSatisfy` (<2)
