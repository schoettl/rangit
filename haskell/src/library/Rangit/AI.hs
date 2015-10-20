module Rangit.AI where

import Rangit.Train
import Rangit.Drive
import Debug.Trace.Extended

type DiscretePath = [Position]

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
backupTrainAccumulateDriveCommands [] _ = []
backupTrainAccumulateDriveCommands path train =
    let (command, newTrain) = backupTrainToFitPath path train
    in command : backupTrainAccumulateDriveCommands (tail path) newTrain

-- | Move train along path fitting the left-most hitch to the first waypoint.
backupTrainToFitPath
    :: DiscretePath          -- ^ Path to fit train to
    -> Train                 -- ^ Train to move
    -> (DriveCommand, Train) -- ^ Best fitted train
backupTrainToFitPath [] _ = error "invalid call: path must not be empty"
backupTrainToFitPath (p:_) train =
    let reversedTrain = reverseTrain (traceShowIdWithMessage "orig.train: " train)
        movedTrain = moveTrainToPosition reversedTrain p
        unreversedTrain = reverseTrain movedTrain
        oldPowerCar = last train
        newPowerCar = last unreversedTrain
        -- besten lenkwinkel und distance herausfinden.
        -- hier kann einiges optimiert werden!
        distance = traceShowId $ - euclidianDistance (partPosition oldPowerCar) (partPosition newPowerCar)
        steerAngle = traceShowId $ calculateSteerAngleToMatchPosition oldPowerCar (calculateCenterPosition newPowerCar)
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

-- | Calculate euclidian distance between two positions.
euclidianDistance (Position x1 y1) (Position x2 y2) = sqrt $ (x2-x1)^2 + (y2-y1)^2
