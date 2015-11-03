module Rangit.Trigonometry
    ( calculateAngleByArcTan
    , calculateMissingTriangleSideABySides
    , calculateMissingTriangleSideAByAngles
    , calculateMissingAngleAlpha
    , calculateAngleOfLine
    , calculateCircumscribedCircleCenter
    , calculateDForCircumscribedCircleCenter
    ) where

import Rangit.Train
import Data.Vector.Extended (Vector2 (Vector2), v2x, v2y)

-- | Calculate an angle using arctan given dx and dy.
calculateAngleByArcTan :: Double -- ^ Delta x
                       -> Double -- ^ Delta y
                       -> Double -- ^ Angle between horizontal line and line defined by dx and dy (counter-clockwise)
calculateAngleByArcTan = flip atan2

-- | Calculate missing triangle side (Law of cosines)
calculateMissingTriangleSideABySides :: Double -> Double -> Double -> Double
calculateMissingTriangleSideABySides alpha b c = sqrt $ b^2 + c^2 + 2*b*c*cos alpha

-- | Calculate missing triangle side (Law of sines)
calculateMissingTriangleSideAByAngles :: Double -> Double -> Double -> Double
calculateMissingTriangleSideAByAngles alpha beta b = sin alpha * b / sin beta

-- | Calculate missing angle in triangle (Law of sines)
calculateMissingAngleAlpha :: Double -> Double -> Double -> Double
calculateMissingAngleAlpha beta a b = asin $ a * sin beta / b

-- | Calculate angle of line between two points.
calculateAngleOfLine
    :: Position -- ^ Start point of line
    -> Position -- ^ End point of line
    -> Double   -- ^ Angle of line between points
calculateAngleOfLine (Vector2 x1 y1) (Vector2 x2 y2) = calculateAngleByArcTan (x2 - x1) (y2 - y1)

-- | Calculate center of circumscribed circle.
-- https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
calculateCircumscribedCircleCenter :: Position -> Position -> Position -> Position
calculateCircumscribedCircleCenter a b c =
    let d = calculateDForCircumscribedCircleCenter a b c
     in Vector2
        (((x a ^2 + y a ^2) * (y b - y c)
                 + (x b ^2 + y b ^2) * (y c - y a)
                 + (x c ^2 + y c ^2) * (y a - y b)) / d)
        (((x a ^2 + y a ^2) * (x c - x b)
                 + (x b ^2 + y b ^2) * (x a - x c)
                 + (x c ^2 + y c ^2) * (x b - x a)) / d)
    where
        x = v2x
        y = v2y

-- | Calculate a helper value d that is used by the function calculateCircumscribedCircleCenter.
-- A property of this function is that it returns 0 if all three points lie on a line.
calculateDForCircumscribedCircleCenter :: Position -> Position -> Position -> Double
calculateDForCircumscribedCircleCenter a b c = 2 * (x a * (y b - y c) + x b * (y c - y a) + x c * (y a - y b))
    where
        x = v2x
        y = v2y
