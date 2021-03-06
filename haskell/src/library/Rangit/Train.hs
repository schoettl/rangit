{-# LANGUAGE CPP #-}

module Rangit.Train
    ( Position
    , Part (..)
    , Train
    , origin
    , fixInitialPositions
    , partLength
    , partHitchDistance
    , trainLength
    , trainPosition
    , calculateCenterPosition
    , calculateLeftHitchPosition
    , translateTrainTo
    , calculatePositionByPointAngleLength
    , reverseTrain
    , validateTrain
    ) where

import Text.Read
import Data.Vector.Extended (Vector2 (Vector2), v2x, v2y)

-- | A position in the map.
type Position = Vector2

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

origin = Vector2 0 0

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

-- | Validate train.
-- 1. Train must have at least one part.
-- 2. Only power car can have it's right hitch behind the axis.
-- 3. No part can have it's right hitch left of it's left hitch.
validateTrain :: Train -> Bool
validateTrain [] = False
validateTrain tr = and (fmap (\p -> partLengthRight p >= 0) (init tr))
                && and (fmap (\p -> partLengthRight p >= - partLengthLeft p) tr)

-- | Calculate position of a part based on it's angle, and angle and position of the car to the right.
calculatePosition :: Part -- ^ Part for which position shall be calculated
                  -> Part -- ^ Part right of with fixed position
                  -> Part -- ^ Part with newly calculated position
calculatePosition part fix = part { partPosition = calculateLeftHitchPosition fix }

partLength :: Part -> Double
partLength p
    | signum a == signum b = a + b
    | otherwise = max (abs a) (abs b)
    where
        a = partLengthLeft p
        b = partLengthRight p

partHitchDistance :: Part -> Double
partHitchDistance p = abs $ partLengthLeft p + partLengthRight p

trainLength :: Train -> Double
trainLength = sum . map partHitchDistance

trainPosition :: Train -> Position
trainPosition = partPosition . last

calculateLeftHitchPosition :: Part -> Position
calculateLeftHitchPosition part = calculatePositionOnPart part $ -partHitchDistance part

calculateCenterPosition :: Part -> Position
calculateCenterPosition part = calculatePositionOnPart part $ -partLengthRight part

calculatePositionOnPart :: Part -> Double -> Position
calculatePositionOnPart part = calculatePositionByPointAngleLength (partPosition part) (partAngle part)

-- | Calculate position given another position, an angle and a length.
calculatePositionByPointAngleLength
    :: Position -- ^ Given position
    -> Double   -- ^ Angle specifying direction of a line from given to resulting position
    -> Double   -- ^ Length specifying length of a line from given to resulting position
    -> Position -- ^ Resulting position
calculatePositionByPointAngleLength p a l = p + Vector2 (l * cos a) (l * sin a)

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
translateTrainTo train targetPosition =
    let vector = targetPosition - trainPosition train
     in map (\ p -> p { partPosition = partPosition p + vector }) train
