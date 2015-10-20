module Debug.Trace.Extended
    ( module Debug.Trace
    , traceShowIdWithMessage
    ) where

import Debug.Trace

-- | Like traceShowId but also logs message along with the value.
traceShowIdWithMessage :: Show a => String -> a -> a
traceShowIdWithMessage msg value = trace (msg ++ show value) value
