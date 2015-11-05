module Data.Vector.ExtendedSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Utils
import Data.Vector.Extended

spec :: Spec
spec = do
    describe "euclidianDistance" $ do
        let a = Vector2 1 1
            b = Vector2 (-1) (-1)
        it "calculates the euclidian distance" $ do
            euclidianDistance a b `shouldBe` (2 * sqrt 2)
