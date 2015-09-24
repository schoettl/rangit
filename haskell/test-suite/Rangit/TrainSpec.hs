module Rangit.TrainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Data.Angle

spec :: Spec
spec = do
    describe "origin" $ do
--    context "when provided a matching pattern" $ do
        it "must be the origin (0, 0)" $ do
            origin @=~? Position 0 0

    describe "calculateCenterPosition" $ do
        let sqrt2 = sqrt 2
        it "works for a simple case" $ do
            calculateLeftHitchPosition (Part origin 0 1 2) @=~? Position (-3) 0
        it "works for non-origin positioned part" $ do
            calculateLeftHitchPosition (Part (Position (-1) 1) 0 1 2) @=~? Position (-4) 1
            --pi @=~? (3.14150000 :: Float) -- test Test.Utils
        it "works with a positive angle" $ do
            calculateLeftHitchPosition (Part origin (Radians $ pi/4) 1 2) @=~? Position (-3/sqrt2) (-3/sqrt2)
        it "works with a positive angle greater 90 degree" $ do
            calculateLeftHitchPosition (Part origin (Radians $ 3/4*pi) 1 2) @=~? Position (3/sqrt2) (-3/sqrt2)
        it "works with a negative angle" $ do
            calculateLeftHitchPosition (Part origin (Radians $ -pi/4) 1 2) @=~? Position (-3/sqrt2) (3/sqrt2)

    describe "calculateCenterPosition" $ do
        let sqrt2 = sqrt 2
            testPart :: Float -> Part
            testPart angle = Part origin (Radians angle) undefined 3
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


-- initial position testen
