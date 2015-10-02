module Rangit.AI where

import Rangit.Train
import Rangit.Drive

type DiscretePath = [Position]
type Train = [Part]

-- | Calculate error by position and angle of power car and by angle of trailing cars.
-- Angles of trailing cars are much more important than angle and position of power car.
-- Angle of a second trailer is more important than angle of first trailer and so on (guess).
calculateError
    :: Train  -- ^ Theoretical train
    -> Train  -- ^ Actual train
    -> Double -- ^ Error
calculateError = 0 -- TODO

-- | Calculate ideal train position for point of path.
calculateTheoreticalTrain
    :: Train        -- ^ Train to be fitted
    -> DiscretePath -- ^ Path to fit train to
    -> Int          -- ^ Index of position to fit train to
    -> Train        -- ^ Ideal position of train
calculateTheoreticalTrain train path index = train

driveTrain
    :: Train        -- ^ Current train
    -> DiscretePath -- ^ Path to fit the train to
    -> Int          -- ^ Index of position to fit the train to
    -> Train        -- ^ Best driven train
driveTrain train path index = []
