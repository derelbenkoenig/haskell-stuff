{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module EDO where


log2 = logBase 2
ratioToSteps edo ratio = edo * log2 ratio
stepsToRatio edo steps = 2 ** (steps / edo)
nearestEdo edo ratio = round (ratioToSteps edo ratio)
centError edo ratio = round ((approximation - exactSteps) / edo * 1200)
    where
    exactSteps = ratioToSteps edo ratio
    approximation = fromInteger (round exactSteps)
majorRatios = [9/8, 5/4, 4/3, 3/2, 5/3, 15/8]
majorScale edo = map (nearestEdo edo) majorRatios
majorError edo = map (centError edo) majorRatios
