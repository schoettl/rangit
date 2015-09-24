module Rangit.Train where

import Data.Angle

-- | A position in the map.
data Position = Position { xPos, yPos :: Float } deriving (Eq, Show)

-- | Part of a vehicle train. It has one axis.
data Part = Part
    { partPosition    :: Position      -- ^ Position of the right hitch
    , partAngle       :: Radians Float -- ^ Angle between West-East (horizontal) line and Längstraverse
    , partLengthLeft  :: Float         -- ^ Length from the axis to the left hitch
    , partLengthRight :: Float         -- ^ Length from the axis to the right hitch. For every part this is the point of force application. It can also be negative to direct to the left side of the axis. This can be especially for the power car.
    }

-- Wheelbase :: Float -- Radabstand
-- CenterDistance :: Float -- Achsabstand

origin = Position 0 0

-- initial positioning --

-- partAngle will not be touched but the hitch position will be corrected!

correctLeftPartsPositions :: [Part] -> [Part]
correctLeftPartsPositions ps = foldr correctPosition [last ps] (init ps)
    where correctPosition part result@(fix:_) =
            let new = calculatePosition part fix
            in new:result

calculatePosition :: Part -- ^ Part for which position shall be calculated
                  -> Part -- ^ Part right of with fixed position
                  -> Part -- ^ Part with newly calculated position
calculatePosition part fix = part { partPosition = calculateLeftHitchPosition fix }

partLength :: Part -> Float
partLength p = partLengthLeft p + partLengthRight p

calculateLeftHitchPosition :: Part -> Position
calculateLeftHitchPosition part = calculatePositionOnPart part $ -partLength part

calculateCenterPosition :: Part -> Position
calculateCenterPosition part = calculatePositionOnPart part $ -partLengthRight part

calculatePositionOnPart :: Part -> Float -> Position
calculatePositionOnPart part = calculatePositionByPointAngleLength (partPosition part) (partAngle part)

calculatePositionByPointAngleLength :: Position -> Radians Float -> Float -> Position
calculatePositionByPointAngleLength (Position x y) a l = Position (x + l * cosine a) (y + l * sine a)
