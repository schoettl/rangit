module Data.Vector.Extended
    ( module V
    , euclidianDistance
    ) where

-- Import Data.Vector from package AC-Vector does not work because cabal or ghc favours package vector.
-- However it works if I specify AC-Vector-Fancy instead of AC-Vector as dependency in the cabal file.
-- but I think the following is the best way:
import Data.Vector.Class as V
import Data.Vector.V2    as V
import Data.Vector.V3    as V

--instance Read Vector2 where
--
--        readPrec =
--        readListPrec =

euclidianDistance :: Vector v => v -> v -> Scalar
euclidianDistance a b = vmag $ b - a
