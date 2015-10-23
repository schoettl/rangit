module Rangit.DriveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.Drive
import Control.Monad
import Data.Tuple.HT

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
                let Position x y = partPosition movedCar
                x `shouldSatisfy` \x -> (x > (1-stepLength)) && x < (1+stepLength)
            it "calculates new angle" $ do
                partAngle movedCar `shouldBe` 0
        context "driving a car to the left (negative distance)" $ do
            let allTrains = driveAccumulateTrains train (-1) 0
                movedTrain = last allTrains
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
                allTrains = driveAccumulateTrains trainWithTrailer (-1) 0
                movedTrain = last allTrains
                movedTrailer:_ = movedTrain
            it "calculates new trailer position" $ do
                let Position x y = partPosition movedTrailer
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
                let Position x y = partPosition movedCar
                x `shouldSatisfy` (<0)
                y `shouldSatisfy` (>0)
            it "calculates new angle correctly" $ do
                partAngle movedCar `shouldSatisfy` (>pi/2)

    describe "moveTrainToPosition" $ do
        context "just a power car" $ do
            let pc = Part origin 0 0 1
                train = [pc]
            it "works for a target in front of" $ do
                let target = Position 1 (-2)
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = - pi/4 }]
            it "works for a target right below the axis center" $ do
                let target = Position (-1) (-1)
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = - pi/2 }]
            it "works for a target behind the train" $ do
                let target = Position (-2) 1
                moveTrainToPosition train target `shouldAlmostBe` [pc { partPosition = target, partAngle = 3*pi/4 }]

    describe "movePart" $ do
        context "move one part" $ do
            let powerCar = Part origin 0 0 1
                targetPosition = Position 1 1
                newAngle = atan $ 1 / (1 + 1) -- 1 from dimensions of power car
                ([pc], _) = movePart powerCar ([], targetPosition)
            it "updates the position" $ do
                partPosition pc `shouldBe` targetPosition
            it "updates the angle" $ do
                partAngle pc `shouldAlmostBe` newAngle
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

    describe "calculateSteerAngleToMatchPosition" $ do
        let car = Part origin 0 undefined 1
        context "using exact circle algorithm for not too large circles" $ do
            let -- circle radius := 1
                otherPoint = Position 0.5 (- sqrt 0.75)
                angleToPartPosition = pi/3 -- inner angles in triangle
                angleOfTangent = angleToPartPosition + pi/2 -- draw it or believe
                steerAngle = - (pi - angleOfTangent)
            it "preconditions for circle algorithm are given" $ do
                calculateDForCircumscrCircleCenter (partPosition car) (calculateCenterPosition car) otherPoint `shouldSatisfy` (>=thresholdForCircleAlgorithm)
            it "works for a horizontal car and a not too large turning circle" $ do
                calculateSteerAngleToMatchPosition car otherPoint `shouldAlmostBeAngle` steerAngle
        context "using simple algorithm for large turning circles" $ do
            let position = Position 10 0.01
            it "preconditions for simple algorithm are given" $ do
                calculateDForCircumscrCircleCenter (partPosition car) (calculateCenterPosition car) position `shouldSatisfy` (<thresholdForCircleAlgorithm)
            it "calculates the simple steer angle correctly" $ do
                calculateSteerAngleToMatchPosition car position `shouldAlmostBe` calculateAngleBetweenPoints origin position

    describe "calculateCircumscribedCircleCenter" $ do
        let a = origin
            b = Position (-1) 0
            c = Position 0.5 (- sqrt 0.75)
            expectedCenter = Position (-0.5) (- sqrt 0.75)
        it "works for simple case" $ do
            calculateCircumscribedCircleCenter a b c `shouldAlmostBe` expectedCenter
        it "is independent of the argument order" $ do
            let points = [a, b, c]
                args = [ (a, b, c) | a <- points, b <- points, c <- points, a /= b && b /= c && c /= a] -- permute does the same but returns a lists and no tuples
            forM_ args $ \ arg ->
                uncurry3 calculateCircumscribedCircleCenter arg `shouldAlmostBe` expectedCenter

    describe "calculateDForCircumscrCircleCenter" $ do
        let a = origin
            b = Position 1 (-1)
            c = Position (-1) 1
            pointOnLine m = Position (m*1) (m*(-1))
        it "equals 0 if points are on a line" $ do
            calculateDForCircumscrCircleCenter a b c `shouldAlmostBe` 0
        it "equals 0 for different a' on the line" $ property $
            \m -> calculateDForCircumscrCircleCenter (pointOnLine m) b c =~ 0
        it "equals 0 for different b' on the line" $ property $
            \m -> calculateDForCircumscrCircleCenter a (pointOnLine m) c =~ 0
        it "equals 0 for different c' on the line" $ property $
            \m -> calculateDForCircumscrCircleCenter a b (pointOnLine m) =~ 0

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
