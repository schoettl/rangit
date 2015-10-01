module Rangit.TrainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Data.Angle

spec :: Spec
spec = do

    describe "AlmostEq from Test.Utils" $ do
        it "compares to floats with accuracy of about 1e-4" $ do
            pi @=~? (3.14150000 :: Double)

    describe "origin" $ do
        it "must be the origin (0, 0)" $ do
            origin @=~? Position 0 0

    describe "partLength" $ do
        it "must be sum of left and right length (hitch distance)" $ do
            partLength (Part undefined undefined 3 2) `shouldBe` 5
        it "must return hitch distance even if right length is negative" $ do
            partLength (Part undefined undefined 3 (-2)) `shouldBe` 1

    describe "calculateLeftHitchPosition" $ do
        let sqrt2 = sqrt 2
        context "positive right length" $ do
            let testPart :: Double -> Part
                testPart angle = Part origin angle 1 2
            it "works for a simple case" $ do
                calculateLeftHitchPosition (testPart 0) @=~? Position (-3) 0
            it "works for non-origin positioned part" $ do
                calculateLeftHitchPosition (Part (Position (-1) 1) 0 1 2) @=~? Position (-4) 1
            it "works with a positive angle" $ do
                calculateLeftHitchPosition (testPart (pi/4)) @=~? Position (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateLeftHitchPosition (testPart (3/4*pi)) @=~? Position (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateLeftHitchPosition (testPart (-pi/4)) @=~? Position (-3/sqrt2) (3/sqrt2)
        context "negative right length" $ do
            let testPartWithNegativeRightLength :: Double -> Part
                testPartWithNegativeRightLength angle = Part origin angle 5 (-2)
            it "works for a simple case" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength 0) @=~? Position (-3) 0
            it "works with a positive angle" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (pi/4)) @=~? Position (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (3/4*pi)) @=~? Position (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateLeftHitchPosition (testPartWithNegativeRightLength (-pi/4)) @=~? Position (-3/sqrt2) (3/sqrt2)
        context "lengths are 0" $ do
            it "works when left length is 0" $ do
                calculateLeftHitchPosition (Part origin 0 0 1) `shouldBe` Position (-1) 0
            it "works when right length is 0" $ do
                calculateLeftHitchPosition (Part origin 0 1 0) `shouldBe` Position (-1) 0
            it "works when both lengths are 0" $ do
                calculateLeftHitchPosition (Part origin 0 0 0) `shouldBe` Position 0 0

    describe "calculateCenterPosition" $ do
        let sqrt2 = sqrt 2
        context "posive right length" $ do
            let testPart :: Double -> Part
                testPart angle = Part origin angle undefined 3
            it "works for a simple case" $ do
                calculateCenterPosition (testPart 0) `shouldBe` Position (-3) 0
            it "works for non-origin positioned part" $ do
                calculateCenterPosition (Part (Position (-1) 1) 0 undefined 3) `shouldBe` Position (-4) 1
            it "works with a positive angle" $ do
                calculateCenterPosition (testPart (pi/4)) @=~? Position (-3/sqrt2) (-3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateCenterPosition (testPart (3/4*pi)) @=~? Position (3/sqrt2) (-3/sqrt2)
            it "works with a negative angle" $ do
                calculateCenterPosition (testPart (-pi/4)) @=~? Position (-3/sqrt2) (3/sqrt2)
        context "negative right length" $ do
            let testPartWithNegativeRightLength :: Double -> Part
                testPartWithNegativeRightLength angle = Part origin angle undefined (-3)
            it "works for a simple case" $ do
                calculateCenterPosition (testPartWithNegativeRightLength 0) `shouldBe` Position 3 0
            it "works with a positive angle" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (pi/4)) @=~? Position (3/sqrt2) (3/sqrt2)
            it "works with a positive angle greater 90 degree" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (3/4*pi)) @=~? Position (-3/sqrt2) (3/sqrt2)
            it "works with a negative angle" $ do
                calculateCenterPosition (testPartWithNegativeRightLength (-pi/4)) @=~? Position (3/sqrt2) (-3/sqrt2)

    describe "fixInitialPositions" $ do
        let powerCar = Part origin ( pi/4) 1 1
            trailer1 = Part origin (-pi/4) 0 2
            trailer2 = Part origin 0       0 0
            t2:t1:pc:[] = fixInitialPositions [trailer2, trailer1, powerCar]
        it "works on a train with just one part" $ do
            fixInitialPositions [powerCar] `shouldBe` [powerCar]
        it "works on a train with three parts" $ do
            partPosition t1 @=~? Position ((-2)/sqrt 2) ((-2)/sqrt 2)
            partPosition t2 @=~? Position (-sqrt 8) 0
        it "does not change the right-most part" $ do
            pc `shouldBe` powerCar
        it "does not change any properties except the position" $ do
            t1 `shouldBe` trailer1 { partPosition = partPosition t1 }
            t2 `shouldBe` trailer2 { partPosition = partPosition t2 }
