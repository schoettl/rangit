module Rangit.TrainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.AI
import Data.Vector.Extended (Vector2 (Vector2), euclidianDistance)

spec :: Spec
spec = do

    describe "AlmostEq from Test.Utils" $ do
        it "compares to floats with accuracy of about 1e-4" $ do
            pi `shouldAlmostBe` (3.14150000 :: Float)

    describe "origin" $ do
        it "must be the origin (0, 0)" $ do
            origin `shouldAlmostBe` Vector2 0 0

    describe "validateTrain" $ do
        let stdPowerCar = Part undefined undefined 0 1
            stdTrailer  = Part undefined undefined 1 3
        it "fails on an empty train" $ do
            validateTrain [] `shouldBe` False
        it "allows power car with right length < 0" $ do
            validateTrain [Part undefined undefined 1 (-1)] `shouldBe` True
        it "allows a standard train" $ do
            validateTrain [stdTrailer, stdPowerCar] `shouldBe` True
        it "fails for trailer with right hitch behind axis" $ do
            validateTrain [Part undefined undefined 2 (-1), stdPowerCar] `shouldBe` False
        it "fails for trailer with left hitch in front of right hitch" $ do
            validateTrain [Part undefined undefined (-2) 1, stdPowerCar] `shouldBe` False
        it "fails for trailer with right hitch behind left hitch" $ do
            validateTrain [Part undefined undefined 1 (-2), stdPowerCar] `shouldBe` False
        it "allows left hitch == right hitch for trailer (right length >= 0)" $ property $
            \ l -> l < 0 || validateTrain [Part undefined undefined (-l) l, stdPowerCar] == True
        it "fails for power car with left hitch in front of right hitch" $ do
            validateTrain [stdTrailer, Part undefined undefined (-2) 1] `shouldBe` False
        it "fails for power car with right hitch behind left hitch" $ do
            validateTrain [stdTrailer, Part undefined undefined 1 (-2)] `shouldBe` False
        it "allows left hitch == right hitch for power car" $ property $
            \ l -> validateTrain [stdTrailer, Part undefined undefined (-l) l] == True

    describe "partHitchDistance" $ do
        it "must be sum of left and right length (hitch distance)" $ do
            partHitchDistance (Part undefined undefined 3 2) `shouldBe` 5
        it "must return hitch distance even if right length is negative" $ do
            partHitchDistance (Part undefined undefined 3 (-2)) `shouldBe` 1

    describe "trainLength" $ do
        it "must equals the sum of it's part lengths" $ do
            let pc = Part undefined undefined 3 2
                tr = Part undefined undefined 5 7
            trainLength [tr, pc] `shouldBe` partHitchDistance tr + partHitchDistance pc

    describe "calculateLeftHitchPosition" $ do
        let sqrt2 = sqrt 2
        context "positive right length" $ do
            let testPart :: Double -> Part
                testPart angle = Part origin angle 1 2
            it "works for a simple case" $ do
                calculateLeftHitchPosition (testPart 0) `shouldAlmostBe` Vector2 (-3) 0
            it "works for non-origin positioned part" $ do
                calculateLeftHitchPosition (Part (Vector2 (-1) 1) 0 1 2) `shouldAlmostBe` Vector2 (-4) 1
            it "works with a positive angle" $ do
                calculateLeftHitchPosition (testPart (pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateLeftHitchPosition (testPart (3/4*pi)) `shouldAlmostBe` Vector2 (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateLeftHitchPosition (testPart (-pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (3/sqrt2)
            it "works for many different angles" $ property $
                \ x -> let p = testPart x
                           d = euclidianDistance (calculateLeftHitchPosition p) (partPosition p)
                       in d =~ partHitchDistance p
        context "negative right length" $ do
            let testPartWithNegativeRightLength :: Double -> Part
                testPartWithNegativeRightLength angle = Part origin angle 5 (-2)
            it "works for a simple case" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength 0) `shouldAlmostBe` Vector2 (-3) 0
            it "works with a positive angle" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (3/4*pi)) `shouldAlmostBe` Vector2 (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (-pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (3/sqrt2)
        context "lengths are 0" $ do
            it "works when left length is 0" $ do
                calculateLeftHitchPosition (Part origin 0 0 1) `shouldBe` Vector2 (-1) 0
            it "works when right length is 0" $ do
                calculateLeftHitchPosition (Part origin 0 1 0) `shouldBe` Vector2 (-1) 0
            it "works when both lengths are 0" $ do
                calculateLeftHitchPosition (Part origin 0 0 0) `shouldBe` Vector2 0 0

    describe "calculateCenterPosition" $ do
        let sqrt2 = sqrt 2
        context "posive right length" $ do
            let testPart :: Double -> Part
                testPart angle = Part origin angle undefined 3
            it "works for a simple case" $ do
                calculateCenterPosition (testPart 0) `shouldBe` Vector2 (-3) 0
            it "works for non-origin positioned part" $ do
                calculateCenterPosition (Part (Vector2 (-1) 1) 0 undefined 3) `shouldBe` Vector2 (-4) 1
            it "works with a positive angle" $ do
                calculateCenterPosition (testPart (pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateCenterPosition (testPart (3/4*pi)) `shouldAlmostBe` Vector2 (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateCenterPosition (testPart (-pi/4)) `shouldAlmostBe` Vector2 (-3/sqrt2) (3/sqrt2)
        context "negative right length" $ do
            let testPartWithNegativeRightLength :: Double -> Part
                testPartWithNegativeRightLength angle = Part origin angle undefined (-3)
            it "works for a simple case" $ do
                calculateCenterPosition (testPartWithNegativeRightLength 0) `shouldBe` Vector2 3 0
            it "works with a positive angle" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (pi/4)) `shouldAlmostBe` Vector2 (3/sqrt2) (3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (3/4*pi)) `shouldAlmostBe` Vector2 (-3/sqrt2) (3/sqrt2)
            it "works with a negative angle" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (-pi/4)) `shouldAlmostBe` Vector2 (3/sqrt2) (-3/sqrt2)

    describe "fixInitialPositions" $ do
        let powerCar = Part origin ( pi/4) 1 1
            trailer1 = Part origin (-pi/4) 0 2
            trailer2 = Part origin 0       0 0
            t2:t1:pc:[] = fixInitialPositions [trailer2, trailer1, powerCar]
        it "works on a train with just one part" $ do
            fixInitialPositions [powerCar] `shouldBe` [powerCar]
        it "works on a train with three parts" $ do
            partPosition t1 `shouldAlmostBe` Vector2 ((-2)/sqrt 2) ((-2)/sqrt 2)
            partPosition t2 `shouldAlmostBe` Vector2 (-sqrt 8) 0
        it "does not change the right-most part" $ do
            pc `shouldBe` powerCar
        it "does not change any properties except the position" $ do
            t1 `shouldBe` trailer1 { partPosition = partPosition t1 }
            t2 `shouldBe` trailer2 { partPosition = partPosition t2 }

    describe "reverseTrain" $ do
        let pc = Part (Vector2 0 1)  0   1 2
            t1 = Part (Vector2 2 3)  pi  3 4
            t2 = Part (Vector2 4 5)  0   5 6
            train = [t2, t1, pc]
            reversedTrain = reverseTrain train
        it "part positions become left hitch positions" $ do
            map partPosition reversedTrain `shouldBe` map calculateLeftHitchPosition (reverse train)
        it "left lengths become right lengths" $ do
            map partLengthLeft reversedTrain `shouldBe` map partLengthRight (reverse train)
        it "right lengths become left lengths" $ do
            map partLengthRight reversedTrain `shouldBe` map partLengthLeft (reverse train)
        it "angles are +180° (because left/right hitches are swaped)" $ do
            map partAngle reversedTrain `shouldBe` map (\ p -> partAngle p + pi) (reverse train)
        it "reverse . reverse == id (how is this property called again?)" $ do
            reverseTrain (reverseTrain train) `shouldAlmostBe` train

    describe "translateTrainTo" $ do
        let pc = Part (Vector2 (-1) (-4)) 1 2 3
            tr = Part (Vector2 (-2) (-3)) 2 3 4
            train = [tr, pc]
            target = Vector2 (-3) (-2)
            newTrPos = Vector2 (-2-3+1) (-3-2+4)
        it "translate part positions correctly" $ do
            translateTrainTo train target `shouldBe` [tr { partPosition = newTrPos }, pc { partPosition = target }]
