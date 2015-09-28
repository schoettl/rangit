module Rangit.DriveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.Drive
import Data.Angle

spec :: Spec
spec = do

    describe "movePart" $ do
        context "move one part" $ do
            let powerCar = Part origin 0 0 1
                targetPosition = Position 1 1
                newAngle = arctangent $ 1 / (1 + 1) -- 1 from dimensions of power car
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
            calculateAngleByArcTan 0 1 `shouldBe` Radians (pi/2)
        it "works for 180°" $ do
            calculateAngleByArcTan (-1) 0 `shouldBe` Radians pi
        it "works for -90°" $ do
            calculateAngleByArcTan 0 (-1) `shouldBe` Radians (-pi/2)
