{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Top8List where
import Data.Kind

newtype Top8List a = Top8List { getTop8List :: [a] }
    deriving (Show, Eq)

pruneTop8 :: Top8List a -> Top8List a
pruneTop8 (Top8List l) = Top8List $ take 8 l

bestN :: Ord a => Int -> [a] -> [a]
bestN n = go [] where
    go bests [] = bests
    go bests (x:xs) = go (insert x bests) xs
    insert = goInsert n
    goInsert k x xs
      | k <= 0 = []
      | otherwise = case xs of
                      [] -> [x]
                      y:ys -> case compare x y of
                                GT -> take k $ x:y:ys
                                EQ -> y:ys
                                LT -> y : goInsert (k - 1) x ys

instance Functor Top8List where
    fmap f (Top8List as) = Top8List $ fmap f as

instance Applicative Top8List where
    pure = pruneTop8 . Top8List . pure
    Top8List fs <*> Top8List xs = pruneTop8 $ Top8List $ fs <*> xs

instance Monad Top8List where
    return = pure
    Top8List l >>= f = Top8List $ l >>= (getTop8List . pruneTop8 . f)
