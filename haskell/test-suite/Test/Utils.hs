{-# LANGUAGE FlexibleInstances #-}

module Test.Utils ((=~), normalizeAngle, shouldAlmostBe, shouldAlmostBeAngle) where

import Test.HUnit as HU
import Rangit.Train
import Rangit.Drive
import Data.Angle
import Data.Vector.Extended (Vector2 (Vector2))

-- from: http://lambda.jstolarek.com/tag/quickcheck/

class AlmostEq a where
    (=~) :: a -> a -> Bool
 
instance AlmostEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

instance AlmostEq Float where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Float)

instance AlmostEq Vector2 where
    Vector2 x1 y1 =~ Vector2 x2 y2 = x1 =~ x2 && y1 =~ y2

instance AlmostEq (Radians Float) where
    Radians x =~ Radians y = x =~ y

instance AlmostEq Part where
    Part (Vector2 x y) a l r =~ Part (Vector2 x' y') a' l' r' =
        x =~ x' &&
        y =~ y' &&
        normalizeAngle a =~ normalizeAngle a' &&
        l =~ l' &&
        r =~ r'

instance AlmostEq Train where
    t =~ t' = all (\ (p, p') -> p =~ p') $ zip t t'
 
-- operator definiton for HSpec:
shouldAlmostBe :: (Show a, AlmostEq a) => a -> a -> HU.Assertion
actual `shouldAlmostBe` expected = actual =~ expected HU.@? assertionMsg
    where assertionMsg = "expected: " ++ show expected
                    ++ "\n but got: " ++ show actual

shouldAlmostBeAngle :: Double -> Double -> HU.Assertion
actual `shouldAlmostBeAngle` expected = normalizeAngle actual =~ normalizeAngle expected HU.@? assertionMsg
    where assertionMsg = "expected: " ++ show expected ++ " = " ++ show (normalizeAngle expected)
                    ++ "\n but got: " ++ show actual   ++ " = " ++ show (normalizeAngle actual)
