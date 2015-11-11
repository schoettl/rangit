{-# LANGUAGE CPP #-}

module Rangit.AI
    ( DiscretePath
    , backupTrainAccumulateDriveCommands
#ifndef TEST
    , calculateAngleInPath
    , calculateIdealTrain
    , calculateError
    , weightAngleDiffs
#endif
    ) where

import Rangit.Train
import Rangit.Drive
import Rangit.Math
import Data.Vector.Extended (Vector2 (Vector2), vdot, euclidianDistance)
import Data.List (minimumBy)

type DiscretePath = [Position]

-- | Maximum steer angle of power car (for backup).
maxSteerAngle :: Double
maxSteerAngle = pi/4

-- | Number of steer angle precision on only one side.
backupSteerPrecision :: Int
backupSteerPrecision = 5

-- | Backup train along the given path accumulating drive commands.
backupTrainAccumulateDriveCommands
    :: DiscretePath   -- ^ Path to backup train along
    -> Train          -- ^ Current train
    -> [DriveCommand] -- ^ Series of drive commands
backupTrainAccumulateDriveCommands path train =
    let correctedPath = removeOverrunnedPoints train path
     in startRecursion correctedPath train
    where
        startRecursion :: DiscretePath -> Train -> [DriveCommand]
        startRecursion [] _ = []
        startRecursion [_] _ = [] -- for backupai strategy 1 there must be more than one point in the path (because of calculateAngleInPath)
        startRecursion path train =
            let (command, newTrain) = backupTrainToFitPath path train in
                command : backupTrainAccumulateDriveCommands (tail path) newTrain

-- | Move train along path fitting the left-most hitch to the first waypoint.
backupTrainToFitPath
    :: DiscretePath          -- ^ Path to fit train to
    -> Train                 -- ^ Train to move
    -> (DriveCommand, Train) -- ^ Best fitted train
backupTrainToFitPath [] _ = error "invalid call: path must not be empty."
backupTrainToFitPath path train =
    let driveCommand@(DriveCommand d a) = calculateBestDriveCommandForBackup1 path train
    in (driveCommand, drive train d a)

calculateBestDriveCommandForBackup2 :: DiscretePath -> Train -> DriveCommand
calculateBestDriveCommandForBackup2 (position:_) train =
    let movedTrain = backupTrainToPosition train position
        oldPowerCar = last train
        newPowerCar = last movedTrain
        -- besten lenkwinkel und distance herausfinden.
        -- hier kann einiges optimiert werden!
        distance = - euclidianDistance (partPosition oldPowerCar) (partPosition newPowerCar)
        steerAngle = calculateSteerAngleForBackup oldPowerCar (calculateLeftHitchPosition newPowerCar)
     in DriveCommand distance steerAngle

-- | Calculate angle in path at given distance from the start.
calculateAngleInPath
    :: DiscretePath -- ^ Path
    -> Double       -- ^ Distance from the path starting point
    -> Double       -- ^ Angle in path at the given distance from starting point
calculateAngleInPath path@(a:b:_) distance = f path (euclidianDistance a b)
    where
        f [a, b] _ = calculateAngleOfLine a b
        f (a:b:c:rest) distToB = if distance < distToB
            then calculateAngleOfLine a b
            else f (b:c:rest) (distToB + euclidianDistance b c)
calculateAngleInPath _ _ = error "path must have at least two points"

removeOverrunnedPoints :: Train -> DiscretePath -> DiscretePath
removeOverrunnedPoints (lastPart:_) = dropWhile badWaypoint
    where
        badWaypoint x =
            let a = partAngle lastPart
                n = Vector2 (cos a) (sin a)
                p = calculateRearAxisPosition lastPart
             in n `vdot` (x - p) > 0

        calculateRearAxisPosition :: Part -> Position
        calculateRearAxisPosition part
            | partLengthRight part < 0 = partPosition lastPart -- special case: rear axis is steer axis (for power car)
            | otherwise                = calculateCenterPosition lastPart

calculateBestDriveCommandForBackup1
    :: DiscretePath -- ^ Path to fit train to
    -> Train        -- ^ Train to move
    -> DriveCommand -- ^ Best drive command
calculateBestDriveCommandForBackup1 path@(p:_) train =
    let distanceToDrive = - euclidianDistance (trainPosition train) p
        idealTrain = calculateIdealTrain path train
        steerAngles = map ((*(maxSteerAngle/fromIntegral backupSteerPrecision)) . fromIntegral) [-backupSteerPrecision..backupSteerPrecision]
        trains = map (drive train distanceToDrive) steerAngles
        diffs = map (calculateError idealTrain) trains

        best :: (Double, Double, Train)
        best = minimumBy (\ (x, _, _) (y, _, _) -> compare x y) $ zip3 diffs steerAngles trains
        (_, bestSteerAngle, _) = best
    in DriveCommand distanceToDrive bestSteerAngle

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
        weightedPositionDiff = weightPositionDiff $ euclidianDistance (getPosition a) (getPosition b)
        getPosition :: Train -> Position
        getPosition = trainPosition --calculateLeftHitchPosition $ head t

-- | Calculate ideal train position for path.
calculateIdealTrain
    :: DiscretePath -- ^ Path to fit train to
    -> Train        -- ^ Train to be fitted
    -> Train        -- ^ Ideal position of train
calculateIdealTrain path train = fixInitialPositions $ foldr f [newPowerCar] (init train)
    where
        calcAngle d = calculateAngleInPath path d - pi
        powerCar = last train
        newPowerCar = powerCar
            { partPosition = head path
            , partAngle = calcAngle $ partLengthRight powerCar
            }
        f :: Part -> Train -> Train
        f p ps = p { partAngle = angle } : ps
            where angle = calcAngle $ partLengthRight p + sum (map partLength ps)

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
