{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PrimeStuff where

data Pow a b = a :^ b
    deriving (Eq, Show)

infixr 6 :^

powBase :: Pow a b -> a
powBase (a :^ _) = a

powExp :: Pow a b -> b
powExp (_ :^ b) = b

-- | returns true if the element is in the ordered (non-decreasing) list
member :: Ord t => t -> [t] -> Bool
member x xs = case xs of
    [] -> False
    y:ys -> case compare x y of
        GT -> member x ys
        EQ -> True
        LT -> False

primes :: [Integer]
primes = sieve [2..] where
    sieve ~(x:xs) = x:sieve (filter ((/= 0) . (`mod` x)) xs)

isPrime :: Integer -> Bool
isPrime x = member x primes

pfactors :: (Eq b, Num b) => Integer -> [Pow Integer b]
pfactors x
    | x < 0 = (-1) :^ 1 : pfactors (-x)
    | x == 0 = []
    | x == 1 = []
    | otherwise = let
        factorItem p n = p :^ countFactor p n
        countFactor p n = let (d,m) = n `divMod` p
                           in if m /= 0 then 0 else 1 + countFactor p d
        potentialPrimeFactors = takeWhile ((<= x) . (^ (2 :: Integer))) primes
        removeEmpties = filter ((/= 0) . powExp)
        in removeEmpties $ map (`factorItem` x) potentialPrimeFactors

arithmeticDerivative :: Integer -> Integer
arithmeticDerivative n
    | n < 0 =  (-1) * arithmeticDerivative (-n)
    | isPrime n = 1
    | otherwise = let
        pf = pfactors n
        component (fac :^ pwr) = pwr * (n `div` fac)
        in sum $ map component pf
