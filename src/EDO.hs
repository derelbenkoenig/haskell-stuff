{-# OPTIONS_GHC -Wno-type-defaults #-}
{-|
Module: EDO
Description: Functions for converting between JI and EDO tunings and measuring how good the
             approximations are

-}
module EDO where

-- * Functions for individual intervals

-- | calcluates how many steps in a given EDO would be the given ratio, as a floating point
ratioToSteps :: (Integral edo, Floating r) => edo -> Rational -> r
ratioToSteps edo ratio = fromIntegral edo * log2 (fromRational ratio)

-- | calculates the ratio (as a floating point number) for the given number of steps in an EDO
stepsToRatio :: (Integral edo, Floating a) => edo -> a -> a
stepsToRatio edo steps = 2 ** (steps / fromIntegral edo)

-- | calculates the nearest whole number of steps in the given EDO that would approximate the given
--   ratio
nearestEdo :: Integral edo => edo -> Rational -> edo
nearestEdo edo ratio = round (ratioToSteps edo ratio)

-- | calculates the difference in cents (i.e. steps in 1200EDO) between the EDO's approximation of
--   the ratio and its exact value.
centError :: (Integral edo, Floating r, RealFrac r) => edo -> Rational -> r
centError edo ratio = roundN 2 ((approximation - exactSteps) / fromIntegral edo * 1200)
    where
    exactSteps = ratioToSteps edo ratio
    approximation = fromIntegral (round exactSteps)

-- * Functions for scales

-- | assume that the desired scale is expressed in JI
type Scale = [Rational]

-- | given a scale in JI, return intervals in the EDO that would approximate that scale
scaleApprox :: Integral edo => edo -> Scale -> [edo]
scaleApprox edo = fmap (nearestEdo edo . fromRational)

-- | given a scale, compute the error in cents for each interval in that scale in the given EDO
scaleError :: (Integral edo, Floating r, RealFrac r, Functor f) => edo -> f Rational -> f r
scaleError edo = fmap (centError edo . fromRational)

-- ** The major (Ionian) scale, included for convenience

-- | the major scale in JI
--
-- >  majorScale = [9/8, 5/4, 4/3, 3/2, 5/3, 15/8]
--
-- This consists of the major second, major third, perfect fourth, perfect fifth, major sixth, and
-- major sevent.
-- For the major sixth, 27\/16 (a fifth above the second) could be used instead of 5\/3 (a major
-- third over the perfet fourth); the conflict between them is one of the main problems that one
-- runs into in Just Intonation since the ii chord or the IV chord will be very out of tune
-- depending on which is chosen.
majorScale :: Scale
majorScale = [9/8, 5/4, 4/3, 3/2, 5/3, 15/8]

-- | give the EDO's approximation for the major scale
majorApprox :: Integral edo => edo -> [edo]
majorApprox edo = scaleApprox edo majorScale

-- | 'scaleError' for the major scale
majorError :: (Integral edo, Floating r, RealFrac r) => edo -> [r]
majorError edo = scaleError edo majorScale

-- * Numeric convenience functions

-- | Takes a list (or any 'Foldable') of values, and gives the square root of the sum of their
--   squares. The function @('deviation' . 'scaleError' edo)@ gives a metric of how "bad" that EDO is
--   for the given scale in JI
deviation :: (Foldable t, Functor t, Floating a) => t a -> a
deviation = sqrt . sum . fmap (^ 2)

-- | Convenience function for building a "table" of input and output values for a function
cell :: (a -> b) -> a -> (a, b)
cell f x = (x, f x)

-- | Convenience function for base 2 log
log2 :: Floating a => a -> a
log2 = logBase 2

-- | convenience function for rounding to @n@ decimal places
roundN :: (RealFrac a, Integral b) => b -> a -> a
roundN n = (/ fac) . fromIntegral . round . (* fac) where
    fac = 10 ^ n
