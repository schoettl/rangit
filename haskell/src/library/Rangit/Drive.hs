{-# LANGUAGE CPP #-}

module Rangit.Drive
    ( DriveCommand (..)
    , stepLength
    , drive
    , driveAccumulateTrains
    , moveTrainToPosition
    , calculateSteerAngleForBackup
    , backupTrainToPosition
#ifndef TEST
    , normalizeAngle
    , modReal
    , movePart
    , thresholdForCircleAlgorithm
    , backupPartToPosition
    , calculateInnerCircleRadius
#endif
    ) where

import Rangit.Train
import Rangit.Math
import Data.Vector.Extended (Vector2 (Vector2), v2x, v2y, euclidianDistance, vlinear)

data DriveCommand = DriveCommand Double Double

-- | Length to drive in one calculation step.
stepLength = 0.01

-- | The result of calculateDForCircumscribedCircleCenter must be greater than this
-- threshold to apply the turning circle algorithm/formulas.
thresholdForCircleAlgorithm = 0.01

-- | API command: drive the train a distance at a steer angle.
drive :: Train  -- ^ Train to be driven
      -> Double -- ^ Distance to be driven (can be positive or negative)
      -> Double -- ^ Steer angle between middle line and direction line, counter-clockwise
      -> Train  -- ^ Train at the new position
drive train len = driveInDirection train (signum len) (abs len)

-- | API command: drive the train a distance at a steer angle. Accumulate all
-- trains i.e. add train after each simulation step. The resulting list is empty
-- if no simulation step is executed.
driveAccumulateTrains
    :: Train   -- ^ Train to be driven
    -> Double  -- ^ Distance to be driven (can be positive or negative)
    -> Double  -- ^ Steer angle between middle line and direction line, counter-clockwise
    -> [Train] -- ^ Trains, one for every step to the new position
driveAccumulateTrains train len = driveInDirectionAccumulateTrains train (signum len) (abs len)

-- | Drive the train a distance at a steer angle.
-- The sign of the distance is needed for the recursion.
driveInDirection :: Train  -- ^ Train to be driven
                 -> Double -- ^ Sign of distance
                 -> Double -- ^ Absolute distance to be driven
                 -> Double -- ^ Steer angle between middle line and direction line, counter-clockwise
                 -> Train  -- ^ Train at the new position
driveInDirection train sign len angle
    | len <= 0  = train
    | otherwise = driveRemaining $ len - stepLength
    where
        driveRemaining len = driveInDirection updated sign len angle
        updated = moveTrain stepLength train sign angle

-- | Drive the train a distance at a steer angle. Accumulate all
-- trains i.e. add train after each simulation step.
-- The sign of the distance is needed for the recursion.
driveInDirectionAccumulateTrains
    :: Train   -- ^ Train to be driven
    -> Double  -- ^ Sign of distance
    -> Double  -- ^ Absolute distance to be driven
    -> Double  -- ^ Steer angle between middle line and direction line, counter-clockwise
    -> [Train] -- ^ Trains, one for every step to the new position
driveInDirectionAccumulateTrains train sign len angle
    | len <= 0  = []
    | otherwise = train : driveRemaining (len - stepLength)
    where
        driveRemaining len = driveInDirectionAccumulateTrains updated sign len angle
        updated = moveTrain stepLength train sign angle

-- | Move the train a given step length.
moveTrain :: Double -- ^ Step length
          -> Train  -- ^ Train to be moved
          -> Double -- ^ Sign for step length denoting the direction
          -> Double -- ^ Steer angle for the power car
          -> Train  -- ^ Moved train
moveTrain stepLength train sign a
    | a < -pi/2 || a > pi/2 = error "invalid steer angle. must be between -90° and +90° (inclusive)."
    | otherwise =
        let point = partPosition $ last train
            angle = a + partAngle (last train)
            target = calculatePositionByPointAngleLength point angle (sign * stepLength)
        in moveTrainToPosition train target

-- | Move the train to a given position in one step.
moveTrainToPosition
    :: Train    -- ^ Train to be moved
    -> Position -- ^ Target train position
    -> Train    -- ^ Moved train
moveTrainToPosition trainParts position = fst $ foldr movePart ([], position) trainParts

-- | Move one part of the train by moving the right hitch position to the target position.
movePart :: Part              -- ^ Current part to be moved
         -> (Train, Position) -- ^ Already moved parts right of current part, and target position for current part
         -> (Train, Position) -- ^ Moved parts including current one and new target position i. e. position of current part's left hitch
movePart part (ps, target) =
    let center = calculateCenterPosition part
        absAngle = calculateAngleOfLine center target
        newPart = part { partPosition = target, partAngle = absAngle }
        leftHitch = calculateLeftHitchPosition newPart
    in (newPart : ps, leftHitch)

-- | Normalize angle so that it is between 0 (inclusive) and 2 pi (exclusive).
normalizeAngle :: Double -> Double
normalizeAngle x = x `modReal` (2*pi)

-- | Modulo operation for instances of Real type class.
modReal :: Real a => a -> a -> a
modReal x m = x - m * fromIntegral (floor $ realToFrac x / realToFrac m)

backupTrainToPosition :: Train -> Position -> Train
backupTrainToPosition train targetPosition = foldl f [] train
    where
        f :: Train -> Part -> Train
        f [] leftmostPart = [backupPartToPosition leftmostPart targetPosition]
        f newTrain part   = newTrain ++ [backupPartToPosition part (partPosition $ last newTrain)]

-- | Back up part by matching the left hitch to the target position.
--
-- New part position can be calculated. Just:
-- zwei kreise schneiden: den inneren kreis und den kreis, der durch l (klein
-- L) um Z beschrieben wird.
-- But there seems to be no explicit formula for this problem.
--
-- Easy workaround (approximation): Just drive train backwards. Distance is
-- assumed to be the direct distance between hitch and new target position.
backupPartToPosition :: Part -> Position -> Part
backupPartToPosition part targetPosition =
    let approximateDistanceToDrive = - euclidianDistance (calculateLeftHitchPosition part) targetPosition
        steerAngle = calculateSteerAngleForBackup part targetPosition
        [newPart] = drive [part] approximateDistanceToDrive steerAngle
     in newPart

-- | Calculate steer angle for backing up part to approach target position. The
-- target position is approached by matching the left hitch to the target
-- position. This refers to Korbinian's sketch.
calculateSteerAngleForBackup :: Part -> Position -> Double
calculateSteerAngleForBackup part targetPosition =
    pi/2 - atan (calculateInnerCircleRadius part targetPosition / partLengthRight part)

-- | Calculate the radius of the circle drawn by the axis center when the part
-- is backing up to approach the target position. The target position is
-- approached by matching the left hitch to the target position. This refers to
-- Korbinian's sketch.
calculateInnerCircleRadius :: Part -> Position -> Double
calculateInnerCircleRadius part newHitchPosition =
    let a = calculateCenterPosition part
        h = calculateLeftHitchPosition part
        z = newHitchPosition
        m = vlinear 0.5 h z
        d = euclidianDistance h z
        l = partLengthLeft part
        angleHZ = calculateAngleOfLine h z
        lenAM = euclidianDistance a m
        beta  = angleHZ - partAngle part
        alpha = calculateMissingTriangleAngleAlpha beta (d/2) lenAM
        gamma = calculateMissingTriangleAngleAlpha beta l     lenAM
        alpha' = pi/2 - alpha
        gamma' = pi/2 - gamma
        beta'  = pi - alpha' - gamma'
        r = calculateMissingTriangleSideAByAngles gamma' beta' lenAM
     in r
