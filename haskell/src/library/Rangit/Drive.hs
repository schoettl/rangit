module Rangit.Drive where

import Rangit.Train
import Debug.Trace.Extended

data DriveCommand = DriveCommand Double Double

-- | Length to drive in one calculation step.
stepLength = 0.01

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
        absAngle = calculateAngleBetweenPoints center target
        leftHitch = calculateLeftHitchPosition part
    in (part { partPosition = target, partAngle = absAngle } : ps, leftHitch)

-- | Calculate an angle using arctan given dx and dy.
calculateAngleByArcTan :: Double -- ^ Delta x
                       -> Double -- ^ Delta y
                       -> Double -- ^ Angle between horizontal line and line defined by dx and dy (counter-clockwise)
calculateAngleByArcTan = flip atan2

-- | Calculate missing triangle side (Law of cosines)
--calculateMissingTriangleSideA :: Radians Double -> Double -> Double -> Double
--calculateMissingTriangleSideA alpha b c = sqrt $ b^2 + c^2 + 2*b*c*cosine alpha

-- | Calculate missing angle in triangle (Law of sines)
--calculateMissingAngleAlpha :: Radians Double -> Double -> Double -> Radians Double
--calculateMissingAngleAlpha beta a b = arcsine $ a * sine beta / b

-- | Calculate angle of line between two points.
calculateAngleBetweenPoints -- TODO fix name!
    :: Position -- ^ Start point of line
    -> Position -- ^ End point of line
    -> Double   -- ^ Angle of line between points
calculateAngleBetweenPoints (Position x1 y1) (Position x2 y2) = calculateAngleByArcTan (x2 - x1) (y2 - y1)

-- | Calculate steer angle to reach the target position.
calculateSteerAngleToMatchPosition
    :: Part     -- ^ Part (part position and part axis center are on the circle)
    -> Position -- ^ Target position (also on the circle)
    -> Double   -- ^ Steer angle for part to reach target position
calculateSteerAngleToMatchPosition part position =
    let a = partPosition part
        b = calculateCenterPosition part
        c = position
        steerAngle = if abs (calculateDForCircumscrCircleCenter a b c) < 0.01
            then calculateAngleBetweenPoints a c - partAngle part
            else calculateSteerAngleFromCircle part position
     in fixSteerAngle steerAngle

calculateSteerAngleFromCircle :: Part -> Position -> Double
calculateSteerAngleFromCircle part position =
    let a = partPosition part
        b = calculateCenterPosition part
        c = position
        center = traceShowIdWithMessage "center point: " $ calculateCircumscribedCircleCenter a b c
        -- Calculate steer angle from tangent of circle
        angleToPartPosition = calculateAngleBetweenPoints center a
        angleOfTangent = angleToPartPosition + pi/2
     in partAngle part - (pi - angleOfTangent)

-- | Calculate center of circumscribed circle.
-- https://en.wikipedia.org/wiki/Circumscribed_circle#Cartesian_coordinates_2
calculateCircumscribedCircleCenter :: Position -> Position -> Position -> Position
calculateCircumscribedCircleCenter a b c =
    let d = calculateDForCircumscrCircleCenter a b c
     in Position
        { xPos = ((x a ^2 + y a ^2) * (y b - y c)
                + (x b ^2 + y b ^2) * (y c - y a)
                + (x c ^2 + y c ^2) * (y a - y b)) / d
        , yPos = ((x a ^2 + y a ^2) * (x c - x b)
                + (x b ^2 + y b ^2) * (x a - x c)
                + (x c ^2 + y c ^2) * (x b - x a)) / d
        }
    where
        x = xPos
        y = yPos

-- | Calculate a helper value d that is used by the function calculateCircumscribedCircleCenter.
-- A property of this function is that it returns 0 if all three points lie on a line.
calculateDForCircumscrCircleCenter :: Position -> Position -> Position -> Double
calculateDForCircumscrCircleCenter a b c = 2 * (x a * (y b - y c) + x b * (y c - y a) + x c * (y a - y b))
    where
        x = xPos
        y = yPos

-- | Fix steer angle if it is outside of the range [-90°, 90°].
-- See function body for details.
fixSteerAngle :: Double -> Double
fixSteerAngle = fix . normalizeAngle
    where
        fix a | a < 0.5*pi = a
              | a > 1.5*pi = a
              | otherwise  = a - pi

normalizeAngle :: Double -> Double
normalizeAngle = mod2pi

mod2pi :: Double -> Double
mod2pi angle = angle - 2*pi * fromIntegral (floor (angle/(2*pi)))
