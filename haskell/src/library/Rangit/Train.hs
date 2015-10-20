module Rangit.Train where

import Text.Read

-- | A position in the map.
data Position = Position { xPos, yPos :: Double } deriving (Eq, Show, Read)

-- | Part of a vehicle train. It has one axis.
data Part = Part
    { partPosition    :: Position -- ^ Position of the right hitch
    , partAngle       :: Double   -- ^ Angle between West-East (horizontal) line and Längstraverse
    , partLengthLeft  :: Double   -- ^ Length from the axis to the left hitch
    , partLengthRight :: Double   -- ^ Length from the axis to the right hitch. For every part this is the point of force application. It can also be negative to direct to the left side of the axis. This can be especially for the power car.
    } deriving (Eq, Show, Read)

type Train = [Part]

-- Wheelbase :: Double -- Radabstand
-- CenterDistance :: Double -- Achsabstand

origin = Position 0 0

-- initial positioning --

-- partAngle will not be touched but the hitch position will be corrected!

-- | Fix positions of all parts except the right-most one.
-- The inital train may have not correct positions of parts.
-- Only the right-most car has a fix position. However the
-- exact position of all other parts can be calculated given
-- the angle, and the position of the car to the right.
fixInitialPositions :: Train -- ^ Train for which positions should be fixed
                    -> Train -- ^ Train with all correct positions
fixInitialPositions ps = foldr correctPosition [last ps] (init ps)
    where correctPosition part result@(fix:_) =
            let new = calculatePosition part fix
            in new:result

-- | Calculate position of a part based on the angle, and the position of the car to the right.
calculatePosition :: Part -- ^ Part for which position shall be calculated
                  -> Part -- ^ Part right of with fixed position
                  -> Part -- ^ Part with newly calculated position
calculatePosition part fix = part { partPosition = calculateLeftHitchPosition fix }

partLength :: Part -> Double
partLength p = partLengthLeft p + partLengthRight p

trainLength :: Train -> Double
trainLength = sum . map partLength

trainPosition :: Train -> Position
trainPosition = partPosition . last

calculateLeftHitchPosition :: Part -> Position
calculateLeftHitchPosition part = calculatePositionOnPart part $ -partLength part

calculateCenterPosition :: Part -> Position
calculateCenterPosition part = calculatePositionOnPart part $ -partLengthRight part

calculatePositionOnPart :: Part -> Double -> Position
calculatePositionOnPart part = calculatePositionByPointAngleLength (partPosition part) (partAngle part)

calculatePositionByPointAngleLength :: Position -> Double -> Double -> Position
calculatePositionByPointAngleLength (Position x y) a l = Position (x + l * cos a) (y + l * sin a)

-- | Reverse the train. The only reason to do this is to virtually drive the
-- train backwards (backupai). This method does not place the power car in the
-- position of the last trailer and so on. It rather sets the driver to the
-- end of the train.
reverseTrain :: Train -> Train
reverseTrain train =
    let train' = map (\ p -> p { partPosition = calculateLeftHitchPosition p
                               , partAngle = partAngle p + pi
                               , partLengthLeft = partLengthRight p
                               , partLengthRight = partLengthLeft p
                               }) train
     in reverse train'

-- | Translate train to a given position. The train position becomes the new
-- position and all other part positions are updated accordingly.
translateTrainTo :: Train -> Position -> Train
translateTrainTo train (Position x y) =
    let trainPos = trainPosition train
        vector = (x - xPos trainPos, y - yPos trainPos)
    in map (\ p -> p { partPosition = translatePosition (partPosition p) vector }) train

translatePosition :: Position -> (Double, Double) -> Position
translatePosition (Position x y) (dx, dy) = Position (x+dx) (y+dy)
