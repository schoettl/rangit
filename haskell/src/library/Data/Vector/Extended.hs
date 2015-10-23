module Data.Vector.Extended
    ( module Data.Vector.Class
    , module Data.Vector.V2
    , module Data.Vector.V3
    , euclidianDistance
    ) where

import Data.Vector.Class
import Data.Vector.V2
import Data.Vector.V3

--instance Read Vector2 where
--
--        readPrec =
--        readListPrec =

euclidianDistance :: Vector v => v -> v -> Scalar
euclidianDistance a b = vmag $ b - a
