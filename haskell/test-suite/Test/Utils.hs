module Test.Utils ((@=~?)) where

import Test.HUnit as HU
import Rangit.Train

-- from: http://lambda.jstolarek.com/tag/quickcheck/

class AlmostEq a where
    (=~) :: a -> a -> Bool
 
instance AlmostEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

instance AlmostEq Float where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Float)

instance AlmostEq Position where
    Position x1 y1 =~ Position x2 y2 = x1 =~ x2 && y1 =~ y2
 
(@=~?) :: (Show a, AlmostEq a) => a -> a -> HU.Assertion
(@=~?) expected actual  = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual
