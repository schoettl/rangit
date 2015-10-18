module Rangit.AI where

import Rangit.Train
import Rangit.Drive
import Data.List
import Data.Ord

type DiscretePath = [Position]

-- | Maximum steer angle of power car (for backup).
maxSteerAngle :: Double
maxSteerAngle = pi/4

-- | Number of steer angle precision on only one side.
backupSteerPrecision :: Int
backupSteerPrecision = 5

-- | Backup train along the given path.
backupTrain
    :: DiscretePath -- ^ Path to backup train along
    -> Train        -- ^ Current train
    -> Train        -- ^ Best driven train
backupTrain []   train = train
backupTrain [_]  train = train
backupTrain path train = backupTrain (tail path) (snd $ backupTrainToFitPath path train)

-- | Backup train along the given path accumulating drive commands.
backupTrainAccumulateDriveCommands
    :: DiscretePath   -- ^ Path to backup train along
    -> Train          -- ^ Current train
    -> [DriveCommand] -- ^ Series of drive commands
backupTrainAccumulateDriveCommands []   train = error "invalid path"
backupTrainAccumulateDriveCommands [_]  train = []
backupTrainAccumulateDriveCommands path train =
    let (command, newTrain) = backupTrainToFitPath path train
    in command : backupTrainAccumulateDriveCommands (tail path) newTrain

-- | Move train to first waypoint so that it best fits the path.
backupTrainToFitPath
    :: DiscretePath          -- ^ Path to fit train to
    -> Train                 -- ^ Train to move
    -> (DriveCommand, Train) -- ^ Best fitted train
backupTrainToFitPath path@(p:_) train =
    let distanceToDrive = -euclidianDistance (partPosition $ last train) p
        idealTrain = calculateIdealTrain path train
        steerAngles = map ((*(maxSteerAngle/fromIntegral backupSteerPrecision)) . fromIntegral) [-backupSteerPrecision..backupSteerPrecision]
        trains = map (drive train distanceToDrive) steerAngles
        diffs = map (calculateError idealTrain) trains

        best :: (Double, Double, Train)
        best = minimumBy (\ (x, _, _) (y, _, _) -> compare x y) $ zip3 diffs steerAngles trains
        (_, bestSteerAngle, bestTrain) = best
    in (DriveCommand distanceToDrive bestSteerAngle, bestTrain)

-- | Calculate error by position and angle of power car and by angle of trailing parts.
-- Angles of trailing parts are much more important than angle and position of power car.
-- Angle of a second trailer is more important than angle of first trailer and so on (guess).
calculateError
    :: Train  -- ^ Theoretical train
    -> Train  -- ^ Actual train
    -> Double -- ^ Error
calculateError a b = weightedPositionDiff + sum weightedAngleDiffs
    where
        angleDiffs = zipWith (\ ap bp -> abs $ partAngle ap - partAngle bp) a b
        weightedAngleDiffs = weightAngleDiffs angleDiffs
        weightedPositionDiff = weightPositionDiff $ euclidianDistance (trainPosition a) (trainPosition b)

-- | Calculate ideal train position for path.
calculateIdealTrain
    :: DiscretePath -- ^ Path to fit train to
    -> Train        -- ^ Train to be fitted
    -> Train        -- ^ Ideal position of train
calculateIdealTrain path train = fixInitialPositions $ foldr f [newPowerCar] (init train)
    where
        calcAngle d = calculateAngleInPath path d - pi
        powerCar = last train
        newPowerCar = powerCar { partPosition = head path
                               , partAngle = calcAngle $ partLengthRight powerCar
                               }
        f :: Part -> Train -> Train
        f p ps = p { partAngle = angle } : ps
            where angle = calcAngle $ partLengthRight p + sum (map partLength ps)

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
    where f d (r, i) = (d * 2^i : r, i+1)

-- | Weight/scale position difference (euclidiance distance).
-- normalisieren: abstand / schlechtest_annehmbarer_abstand = winkel_mean / schlechtest_annehmbarer_winkel_zb360
weightPositionDiff
    :: Double -- ^ Euclidian distance as difference between different positioned trains
    -> Double -- ^ Weighted position difference
weightPositionDiff x =
    let acceptablePositionDiff = 2
        acceptableAngleDiff = pi/4
    in x / acceptablePositionDiff * acceptableAngleDiff
