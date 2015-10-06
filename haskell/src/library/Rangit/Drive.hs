module Rangit.Drive where

import Rangit.Train

-- | Length to drive in one calculation step.
stepLength = 0.01

-- | API command: drive the train a distance at a steer angle.
drive :: Train  -- ^ Train to be driven
      -> Double -- ^ Distance to be driven (can be positive or negative)
      -> Double -- ^ Steer angle between middle line and direction line, counter-clockwise
      -> Train  -- ^ Train at the new position
drive train len = driveInDirection train (signum len) (abs len)

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
        updated = moveTrain train sign angle

-- | Move the train one step length.
moveTrain :: Train  -- ^ Train to be moved
          -> Double -- ^ Sign for step length denoting the direction
          -> Double -- ^ Steer angle for the power car
          -> Train  -- ^ Moved train
moveTrain ps sign a =
    let point = partPosition $ last ps
        angle = a + partAngle (last ps)
        target = calculatePositionByPointAngleLength point angle (sign * stepLength)
    in fst $ foldr movePart ([], target) ps

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
calculateAngleBetweenPoints
    :: Position -- ^ Start point of line
    -> Position -- ^ End point of line
    -> Double   -- ^ Angle of line between points
calculateAngleBetweenPoints (Position x1 y1) (Position x2 y2) = calculateAngleByArcTan (x2 - x1) (y2 - y1)
