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
        context "angle is calculated at axis" $ do
            let path = [Position 0 0, Position (-1) 0, Position (-1) (-1)]
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
            partPositionDiffProp train part x = Position x x == partPosition part || calculateError idealTrain (fixInitialPositions train) > noDiff
            partAngleDiffProp train part x = x == partAngle part || calculateError idealTrain (fixInitialPositions train) > noDiff
        it "is no difference between two times the same train" $ do
            noDiff `shouldBe` 0
        it "looks good for different positions of power car" $ property $
           \ x -> partPositionDiffProp [tr2, tr1, pc { partPosition = Position x x }] pc x
        it "looks good for different angle of just the power car" $ do
            calculateError [pc] [pc { partAngle = pi }] `shouldNotBe` noDiff
        it "looks good for different angles of power car" $ property $
           \ x -> partAngleDiffProp [tr2, tr1, pc { partAngle = x }] pc x
        it "looks good for different angles of trailer 1" $ property $
           \ x -> partAngleDiffProp [tr2, tr1 { partAngle = x }, pc] tr1 x
        it "looks good for different angles of trailer 2" $ property $
           \ x -> partAngleDiffProp [tr2 { partAngle = x }, tr1, pc] tr2 x

    describe "backupTrainToFitPath" $ do
        let path = [origin, Position (-1) 0]
            pc = Part origin 0 1 1
        context "only power car" $ do
            let idealTrain = [pc]
                betterFitProp x
                    | x <   1 = True
                    | x > 100 = True -- sonst dauerts so lange, weil er über 100 m in cm Schritten simulieren muss, mehrfach pro Test
                    | otherwise = calculateError idealTrain movedTrain
                                < calculateError idealTrain train
                    where
                        train = [pc { partPosition = Position x x }]
                        movedTrain = snd $ backupTrainToFitPath path train
            it "moves the train to fit the ideal train better" $ property $
                betterFitProp
        context "power car with one trailer" $ do
            let tr = Part origin 0 1 1
                idealTrain = fixInitialPositions [tr, pc]
                betterFitProp x
                    | x <   1 = True
                    | x > 100 = True -- sonst dauerts so lange, weil er über 100 m in cm Schritten simulieren muss, mehrfach pro Test
                    | otherwise = calculateError idealTrain movedTrain
                                < calculateError idealTrain train
                    where
                        train = fixInitialPositions [tr, pc { partPosition = Position x x }]
                        movedTrain = snd $ backupTrainToFitPath path train
            it "moves the train to fit the ideal train better" $ property $
                betterFitProp

    let secondFromEnd = last . init
        trainPosition = partPosition . last
        path = [origin, Position (-1) 0, Position (-2) (-1)]
        train = fixInitialPositions [Part origin 0 0 1, Part (Position 2 0) 0 1 1]
    describe "backupTrain" $ do
        let newTrain = backupTrain path train
            target = secondFromEnd path
        it "approaches the target i.e. second point of path from the end" $ do
            euclidianDistance (trainPosition newTrain) target `shouldSatisfy` (< euclidianDistance (trainPosition train) target)
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
