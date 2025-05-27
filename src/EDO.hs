{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module EDO where

log2 :: Floating a => a -> a
log2 = logBase 2

roundN :: (RealFrac a, Integral b) => b -> a -> a
roundN n = (/ fac) . fromIntegral . round . (* fac) where
    fac = 10 ^ n

ratioToSteps :: (Integral edo, Floating r) => edo -> r -> r
ratioToSteps edo ratio = fromIntegral edo * log2 ratio

stepsToRatio :: (Integral edo, Floating a) => edo -> a -> a
stepsToRatio edo steps = 2 ** (steps / fromIntegral edo)

nearestEdo :: (Integral edo, Floating a, RealFrac a) => edo -> a -> edo
nearestEdo edo ratio = round (ratioToSteps edo ratio)

centError :: (Integral edo, Floating r, RealFrac r) => edo -> r -> r
centError edo ratio = roundN 2 ((approximation - exactSteps) / fromIntegral edo * 1200)
    where
    exactSteps = ratioToSteps edo ratio
    approximation = fromIntegral (round exactSteps)

majorRatios :: [Rational]
majorRatios = [9/8, 5/4, 4/3, 3/2, 5/3, 15/8]

majorScale :: Integral edo => edo -> [edo]
majorScale edo = map (nearestEdo edo) (fromRational <$> majorRatios)

majorError :: (Integral edo, Floating r, RealFrac r) => edo -> [r]
majorError edo = map (centError edo) (fromRational <$> majorRatios)

sse :: (Foldable t, Functor t, Floating a) => t a -> a
sse = sqrt . sum . fmap (^ 2)
