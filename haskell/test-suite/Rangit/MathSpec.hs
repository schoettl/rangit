module Rangit.MathSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Rangit.Train
import Rangit.Math
import Data.Vector.Extended (Vector2 (Vector2))
import Control.Monad
import Data.Tuple.HT

spec :: Spec
spec = do

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

    describe "calculateCircumscribedCircleCenter" $ do
        let a = origin
            b = Vector2 (-1) 0
            c = Vector2 0.5 (- sqrt 0.75)
            expectedCenter = Vector2 (-0.5) (- sqrt 0.75)
        it "works for simple case" $ do
            calculateCircumscribedCircleCenter a b c `shouldAlmostBe` expectedCenter
        it "is independent of the argument order" $ do
            let points = [a, b, c]
                args = [ (a, b, c) | a <- points, b <- points, c <- points, a /= b && b /= c && c /= a] -- permute does the same but returns a lists and no tuples
            forM_ args $ \ arg ->
                uncurry3 calculateCircumscribedCircleCenter arg `shouldAlmostBe` expectedCenter

    describe "calculateDForCircumscribedCircleCenter" $ do
        let a = origin
            b = Vector2 1 (-1)
            c = Vector2 (-1) 1
            pointOnLine m = Vector2 (m*1) (m*(-1))
        it "equals 0 if points are on a line" $ do
            calculateDForCircumscribedCircleCenter a b c `shouldAlmostBe` 0
        it "equals 0 for different a' on the line" $ property $
            \m -> calculateDForCircumscribedCircleCenter (pointOnLine m) b c =~ 0
        it "equals 0 for different b' on the line" $ property $
            \m -> calculateDForCircumscribedCircleCenter a (pointOnLine m) c =~ 0
        it "equals 0 for different c' on the line" $ property $
            \m -> calculateDForCircumscribedCircleCenter a b (pointOnLine m) =~ 0
