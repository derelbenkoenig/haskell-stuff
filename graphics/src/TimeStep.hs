{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module TimeStep where


type TimeValue t = (Ord t, Fractional t)

-- TODO this may not be sufficient as the update may depend on both the current
-- time and time delta, not just the delta
class TimeSteppable a where
    timeStep :: forall t. TimeValue t => t -> a -> a

instance TimeSteppable () where
    timeStep _ = id

