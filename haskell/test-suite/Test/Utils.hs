{-# LANGUAGE FlexibleInstances #-}

module Test.Utils (shouldAlmostBe, (@=~?)) where

import Test.HUnit as HU
import Rangit.Train
import Data.Angle

-- from: http://lambda.jstolarek.com/tag/quickcheck/

class AlmostEq a where
    (=~) :: a -> a -> Bool
 
instance AlmostEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

instance AlmostEq Float where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Float)

instance AlmostEq Position where
    Position x1 y1 =~ Position x2 y2 = x1 =~ x2 && y1 =~ y2

instance AlmostEq (Radians Float) where
    Radians x =~ Radians y = x =~ y
 
-- operator definiton for HSpec:
shouldAlmostBe :: (Show a, AlmostEq a) => a -> a -> HU.Assertion
actual `shouldAlmostBe` expected = actual =~ expected HU.@? assertionMsg
    where assertionMsg = "expected: " ++ show expected
                    ++ "\n but got: " ++ show actual

-- deprecated:
x @=~? y = shouldAlmostBe x y
