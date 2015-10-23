module Data.Vector.Extended
    ( module V
    , euclidianDistance
    ) where

import Data.Vector.Class as V
import Data.Vector.V2    as V
import Data.Vector.V3    as V

--instance Read Vector2 where
--
--        readPrec =
--        readListPrec =

euclidianDistance :: Vector v => v -> v -> Scalar
euclidianDistance a b = vmag $ b - a
