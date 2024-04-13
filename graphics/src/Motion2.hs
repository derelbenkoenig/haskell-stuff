{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Motion2 where

import SDL.Vect

class HasMotion n a where
    getVelocity :: a -> Velocity n
    getPosition :: a -> Position n
    getAcceleration :: a -> Acceleration n
    withVelocity :: Velocity n -> a -> a
    withPosition :: Position n -> a -> a
    withAcceleration :: Acceleration n -> a -> a

newtype Position a = Position (V2 a)

newtype Velocity a = Velocity (V2 a)

newtype Acceleration a = Acceleration (V2 a)

applyVelocity :: Fractional a =>
                 a -- ^ time delta
                 -> Velocity a -- ^ velocity
                 -> Position a -- ^ initial position
                 -> Position a -- ^ final position
applyVelocity t (Velocity v) (Position p) =
    Position $ (+) <$> p <*> ((* t) <$> v)
