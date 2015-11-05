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

    describe "weightedAngleDiffs" $ do
        it "weights angle differences with even exponents" $ do
            weightAngleDiffs [4, 3, 2] `shouldBe` [16, 6, 2]

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

    describe "calculateIdealTrain" $ do
        let path = [Vector2 0 0, Vector2 2 2, Vector2 3 2]
            initPosition = Vector2 (-1) (-1)
            powerCar = Part initPosition 0 1 1
        context "just the power car" $ do
            let [powerCar'] = calculateIdealTrain path [powerCar]
            it "has correct position" $ do
                partPosition powerCar' `shouldBe` Vector2 0 0
            it "has correct angle" $ do
                partAngle powerCar' `shouldBe` -3/4*pi
        context "power car and trailer" $ do
            let trailer = Part initPosition 0 1 1
                [tr, pc] = calculateIdealTrain path [trailer, powerCar]
            it "power car has correct position" $ do
                partPosition pc `shouldBe` Vector2 0 0
            it "power car has correct angle" $ do
                partAngle pc `shouldBe` -3/4*pi
            it "trailer has correct position" $ do
                partPosition tr `shouldAlmostBe` Vector2 (sqrt 2) (sqrt 2)
            it "trailer has correct angle" $ do
                -- -pi oder +pi, hauptsache 180° verdreht halt
                partAngle tr `shouldBe` -pi
        context "angle is calculated at axis" $ do
            let path = [Vector2 0 0, Vector2 (-1) 0, Vector2 (-1) (-1)]
                angleOfAdjustedPowerCar rightLength = partAngle $ head $ calculateIdealTrain path [powerCar { partLengthRight = rightLength }]
            it "is horizontal when distance from axis to right hitch is less than 1" $ do
                angleOfAdjustedPowerCar 0.9 `shouldAlmostBeAngle` 0
            it "is vertical when distance from axis to right hitch is equal to 1" $ do
                angleOfAdjustedPowerCar 1.0 `shouldAlmostBeAngle` (pi/2)
            it "is vertical when distance from axis to right hitch is greater than 1" $ do
                angleOfAdjustedPowerCar 1.1 `shouldAlmostBeAngle` (pi/2)

    describe "calculateError" $ do
        let pc  = Part origin (pi/2) 1 1
            tr1 = Part origin (pi/4) 1 1
            tr2 = Part origin (pi/8) 1 1
            idealTrain = fixInitialPositions [tr2, tr1, pc]
            noDiff = calculateError idealTrain idealTrain
            partPositionDiffProp train part x = Vector2 x x == partPosition part || calculateError idealTrain (fixInitialPositions train) > noDiff
            partAngleDiffProp train part x = x == partAngle part || calculateError idealTrain (fixInitialPositions train) > noDiff
        it "is no difference between two times the same train" $ do
            noDiff `shouldBe` 0
        it "looks good for different positions of power car" $ property $
           \ x -> partPositionDiffProp [tr2, tr1, pc { partPosition = Vector2 x x }] pc x
        it "looks good for different angle of just the power car" $ do
            calculateError [pc] [pc { partAngle = pi }] `shouldNotBe` noDiff
        it "looks good for different angles of power car" $ property $
           \ x -> partAngleDiffProp [tr2, tr1, pc { partAngle = x }] pc x
        it "looks good for different angles of trailer 1" $ property $
           \ x -> partAngleDiffProp [tr2, tr1 { partAngle = x }, pc] tr1 x
        it "looks good for different angles of trailer 2" $ property $
           \ x -> partAngleDiffProp [tr2 { partAngle = x }, tr1, pc] tr2 x

    describe "backupTrainAccumulateDriveCommands" $ do
        let train = fixInitialPositions [Part origin pi 0 1, Part origin pi 1 1]
            path = [Vector2 5 0, Vector2 6 0, Vector2 7 (-1)]
            result = backupTrainAccumulateDriveCommands path train
        it "should return as many drive commands as there are points in path,\
           \as long as no waypoints are overrunned" $ do
            length result `shouldBe` length path
