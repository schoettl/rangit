module Rangit.AI where

import Rangit.Train
import Rangit.Drive

type DiscretePath = [Position]

-- | Calculate error by position and angle of power car and by angle of trailing cars.
-- Angles of trailing cars are much more important than angle and position of power car.
-- Angle of a second trailer is more important than angle of first trailer and so on (guess).
calculateError
    :: Train  -- ^ Theoretical train
    -> Train  -- ^ Actual train
    -> Double -- ^ Error
calculateError a b = powerCarPositionDiffScaled + sum weightedAngleDiffs
    where
        angleDiffs = map (\ (ap, bp) -> partAngle ap - partAngle bp) (init $ zip a b)
        weightedAngleDiffs = weightAngleDiffs angleDiffs
        powerCarPositionDiffScaled = euclidianDistance (partPosition $ last a) (partPosition $ last b) / 0.5 * 2*pi -- normalisieren: abstand / schlechtest_annehmbarer_abstand = winkel_mean / schlechtest_annehmbarer_winkel_zb360

-- | Calculate ideal train position for path.
calculateTheoreticalTrain
    :: DiscretePath -- ^ Path to fit train to
    -> Train        -- ^ Train to be fitted
    -> Train        -- ^ Ideal position of train
calculateTheoreticalTrain path train = fixInitialPositions $ foldr f [newPowerCar] (init train)
    where
        calcAngle = calculateAngleInPath path
        powerCar = last train
        newPowerCar = powerCar { partPosition = head path
                               , partAngle = calcAngle $ partLengthRight powerCar
                               }
        f :: Part -> Train -> Train
        f p ps = p { partAngle = angle } : ps
            where angle = calcAngle $ partLengthRight p + sum (map partLength ps)

-- | Backup train along the given path.
backupTrain
    :: Train        -- ^ Current train
    -> DiscretePath -- ^ Path to backup train along
    -> Train        -- ^ Best driven train
backupTrain train path = []

-- | Calculate angle in path at given distance from the start.
calculateAngleInPath
    :: DiscretePath -- ^ Path
    -> Double       -- ^ Distance from the path starting point
    -> Double       -- ^ Angle in path at the given distance from starting point
calculateAngleInPath path@(a:b:_) distance = f path (euclidianDistance a b)
    where
        f [a, b] _ = calculateAngleBetweenPoints a b
        f (a:b:c:rest) distToB = if distance < distToB
            then calculateAngleBetweenPoints a b
            else f (b:c:rest) (distToB + euclidianDistance b c)
calculateAngleInPath _ _ = error "path must have at least two points"

-- | Calculate euclidian distance between two positions.
euclidianDistance (Position x1 y1) (Position x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2

-- | Weight angle differences of parts. The right-most part is the power car.
weightAngleDiffs
    :: [Double] -- ^ Angle differences between two different positioned trains
    -> [Double] -- ^ Weighted angle differences
weightAngleDiffs list = fst $ foldr f ([], 0) list
    where f d (r, i) = (d ^ (2*i) : r, i+1)
