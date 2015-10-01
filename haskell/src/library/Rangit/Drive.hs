module Rangit.Drive where

import Rangit.Train

-- | Length to drive in one calculation step.
stepLength = 0.01

-- | API command: drive the train a distance at a steer angle.
drive :: [Part] -- ^ Train to be driven
      -> Float  -- ^ Distance to be driven (can be positive or negative)
      -> Float  -- ^ Steer angle between middle line and direction line, counter-clockwise
      -> [Part] -- ^ Train at the new position
drive train len = driveInDirection train (signum len) (abs len)

-- | Drive the train a distance at a steer angle.
-- The sign of the distance is needed for the recursion.
driveInDirection :: [Part] -- ^ Train to be driven
                 -> Float  -- ^ Sign of distance
                 -> Float  -- ^ Absolute distance to be driven
                 -> Float  -- ^ Steer angle between middle line and direction line, counter-clockwise
                 -> [Part] -- ^ Train at the new position
driveInDirection train sign len angle
    | len <= 0  = train
    | otherwise = driveRemaining $ len - stepLength
    where
        driveRemaining len = driveInDirection updated sign len angle
        updated = moveTrain train sign angle

-- | Move the train one step length.
moveTrain :: [Part] -- ^ Train to be moved
          -> Float  -- ^ Sign for step length denoting the direction
          -> Float  -- ^ Steer angle for the power car
          -> [Part] -- ^ Moved train
moveTrain ps sign a =
    let point = partPosition $ last ps
        angle = a + partAngle (last ps)
        target = calculatePositionByPointAngleLength point angle (sign * stepLength)
    in fst $ foldr movePart ([], target) ps

-- | Move one part of the train by moving the right hitch position to the target position.
movePart :: Part               -- ^ Current part to be moved
         -> ([Part], Position) -- ^ Already moved parts right of current part, and target position for current part
         -> ([Part], Position) -- ^ Moved parts including current one and new target position i. e. position of current part's left hitch
movePart part (ps, target) =
    let center = calculateCenterPosition part
        xDiff = xPos target - xPos center
        yDiff = yPos target - yPos center
        absAngle = calculateAngleByArcTan xDiff yDiff
        leftHitch = calculateLeftHitchPosition part
    in (part { partPosition = target, partAngle = absAngle } : ps, leftHitch)

-- | Calculate an angle using arctan given dx and dy.
calculateAngleByArcTan :: Float -- ^ Delta x
                       -> Float -- ^ Delta y
                       -> Float -- ^ Angle between horizontal line and line defined by dx and dy (counter-clockwise)
calculateAngleByArcTan = flip atan2

-- | Calculate missing triangle side (Law of cosines)
--calculateMissingTriangleSideA :: Radians Float -> Float -> Float -> Float
--calculateMissingTriangleSideA alpha b c = sqrt $ b^2 + c^2 + 2*b*c*cosine alpha

-- | Calculate missing angle in triangle (Law of sines)
--calculateMissingAngleAlpha :: Radians Float -> Float -> Float -> Radians Float
--calculateMissingAngleAlpha beta a b = arcsine $ a * sine beta / b
