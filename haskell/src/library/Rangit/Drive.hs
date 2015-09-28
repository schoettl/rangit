module Rangit.Drive where

import Rangit.Train

-- | Length to drive in one calculation step.
stepLength = 0.01

-- | API command: drive the train a distance at a steer angle.
drive :: [Part] -- ^ Train to be driven
      -> Float  -- ^ Distance to be driven
      -> Float  -- ^ Steer angle between middle line and direction line, counter-clockwise
      -> [Part] -- ^ Train at the new position
drive train len angle
    | len <= 0  = train
    | otherwise = drive updated (len - stepLength) angle
        where updated = moveTrain train angle

-- | Move the train one step length.
moveTrain :: [Part] -- ^ Train to be moved
          -> Float  -- ^ Steer angle for the power car
          -> [Part] -- ^ Moved train
moveTrain ps a =
    let point = partPosition $ last ps
        angle = a + partAngle (last ps)
        target = calculatePositionByPointAngleLength point angle stepLength
    in fst $ foldr movePart ([], target) ps

-- | Move one part of the train by moving the right hitch position to the target position.
movePart :: Part               -- ^ Current part to be moved
         -> ([Part], Position) -- ^ Already moved parts right of current part, and target position for current part
         -> ([Part], Position) -- ^ Moved parts including current one and new target position i. e. position of current part's left hitch
movePart part (ps, target) =
    let center = calculateCenterPosition part
        yDiff = yPos target - yPos center
        xDiff = xPos target - xPos center
        absAngle = calculateAngleByArcTan xDiff yDiff
    in (part { partPosition = target, partAngle = absAngle } : ps, calculateLeftHitchPosition part)

-- | Calculate an angle using arctan given dx and dy.
calculateAngleByArcTan :: Float -> Float -> Float
calculateAngleByArcTan xDiff yDiff = if xDiff /= 0
    then decideForY  xDiff yDiff
    else decideForX0 xDiff yDiff
    where
        decideForY xDiff yDiff
            | yDiff == 0 && xDiff > 0 = 0
            | yDiff == 0 && xDiff < 0 = pi
            | otherwise = atan (yDiff / xDiff)
        decideForX0 xDiff yDiff
            | yDiff > 0 = pi/2
            | yDiff < 0 = -pi/2


-- | Calculate missing triangle side (Law of cosines)
--calculateMissingTriangleSideA :: Radians Float -> Float -> Float -> Float
--calculateMissingTriangleSideA alpha b c = sqrt $ b^2 + c^2 + 2*b*c*cosine alpha

-- | Calculate missing angle in triangle (Law of sines)
--calculateMissingAngleAlpha :: Radians Float -> Float -> Float -> Radians Float
--calculateMissingAngleAlpha beta a b = arcsine $ a * sine beta / b
