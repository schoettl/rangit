module Rangit.Drive where

import Data.Angle
import Rangit.Train

-- driving --

-- | Length to drive in one calculation step.
stepLength = 0.01

-- | API command: drive the train a distance at a steer angle.
drive :: [Part]        -- ^ Train to be driven
      -> Float         -- ^ Distance to be driven
      -> Radians Float -- ^ Steer angle between middle line and direction line, counter-clockwise
      -> [Part]        -- ^ Train at the new position
drive train len angle
    | len <= 0  = train
    | otherwise = drive updated (len - stepLength) angle
        where updated = moveTrain train angle

-- | Move the train one step length.
moveTrain :: [Part]        -- ^ Train to be moved
          -> Radians Float -- ^ Steer angle for the power car
          -> [Part]        -- ^ Moved train
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
        absAngle = arctangent $ (yPos target - yPos center) / (xPos target - xPos center)
    in (part { partAngle = absAngle } : ps, calculateLeftHitchPosition part)


-- | Calculate missing triangle side (Law of cosines)
--calculateMissingTriangleSideA :: Radians Float -> Float -> Float -> Float
--calculateMissingTriangleSideA alpha b c = sqrt $ b^2 + c^2 + 2*b*c*cosine alpha

-- | Calculate missing angle in triangle (Law of sines)
--calculateMissingAngleAlpha :: Radians Float -> Float -> Float -> Radians Float
--calculateMissingAngleAlpha beta a b = arcsine $ a * sine beta / b
