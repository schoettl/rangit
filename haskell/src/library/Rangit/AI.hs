{-# LANGUAGE CPP #-}

module Rangit.AI
    ( DiscretePath
    , backupTrainAccumulateDriveCommands
#ifndef TEST
    , calculateAngleInPath
#endif
    ) where

import Rangit.Train
import Rangit.Drive
-- import Data.Vector from package AC-Vector does not work because cabal favours package vector
import Data.Vector.Extended (Vector2 (Vector2), vdot, euclidianDistance)

type DiscretePath = [Position]

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
        startRecursion path train =
            let (command, newTrain) = backupTrainToFitPath path train in
                command : backupTrainAccumulateDriveCommands (tail path) newTrain

-- | Move train along path fitting the left-most hitch to the first waypoint.
backupTrainToFitPath
    :: DiscretePath          -- ^ Path to fit train to
    -> Train                 -- ^ Train to move
    -> (DriveCommand, Train) -- ^ Best fitted train
backupTrainToFitPath [] _ = error "invalid call: path must not be empty."
backupTrainToFitPath (p:_) train =
    let reversedTrain = reverseTrain train
        movedTrain = moveTrainToPosition reversedTrain p
        unreversedTrain = reverseTrain movedTrain
        oldPowerCar = last train
        newPowerCar = last unreversedTrain
        -- besten lenkwinkel und distance herausfinden.
        -- hier kann einiges optimiert werden!
        distance = - euclidianDistance (partPosition oldPowerCar) (partPosition newPowerCar)
        steerAngle = calculateSteerAngleToMatchPosition oldPowerCar (calculateCenterPosition newPowerCar)
        -- Set drive command and drive train
        driveCommand = DriveCommand distance steerAngle
        drivenTrain = drive train distance steerAngle
    in (driveCommand, drivenTrain)

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
