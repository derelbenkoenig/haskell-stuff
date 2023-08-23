{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module AlgebraStuff where

import Control.Monad
import Data.Functor.Compose

{- catamorphism on algebra

              fmap (cata alg)
    f (Fix f) ---------> f a
       ^                  |
       |unfix             |alg
       |     cata alg     v
     Fix f ------------>  a
-}

type Algebra f a = f a -> a

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance (forall a. Show a => Show (f a)) => Show (Fix f)

data NatF a = ZeroF | SuccF a
    deriving (Functor, Show)

instance Foldable NatF where
    foldr f z n = case n of
        ZeroF -> z
        SuccF a -> f a z

instance Traversable NatF where
    traverse g na = case na of
        ZeroF -> pure ZeroF
        SuccF a -> fmap SuccF (g a)

type Nat = Fix NatF

ntimes n f = if n <= 0 then id else \x -> f ((ntimes (n-1) f) x)

natFromInt n = ntimes n (Fix . SuccF) (Fix ZeroF)
natToInt n = case n of
    Fix ZeroF -> 0
    Fix (SuccF m) -> 1 + natToInt m

fib :: Algebra NatF (Int, Int)
fib ZeroF = (0, 1)
fib (SuccF (a, b)) = (b, a + b)

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

{- catamorphism on algebra, in kleisli category

              f (cataK kalg)
    f (Fix f) ---------> f a
       ^                  |
       |return . unfix    | kalg :: f a -> m a
       |     cataK kalg   v
     Fix f ------------>  a
-}

type KleisliAlgebra m f a = f a -> m a

cataKleisli :: forall m f a. (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataKleisli kalg = kalg <=< (liftCataKleisli kalg) <=< (return . unFix)

liftCataKleisli :: forall m f a. (Traversable f, Monad m) => (f a -> m a) -> f (Fix f) -> m (f a)
liftCataKleisli kalg = traverse (cataKleisli kalg)

printAndReturn :: Show a => a -> IO a
printAndReturn a = print a >> return a

printingAlg :: (Functor f, Show a) => Algebra f a -> KleisliAlgebra IO f a
printingAlg alg fa = let a = alg fa in printAndReturn a

printingFib = printingAlg fib

-- e.g. 
-- ghci> void $ cataKleisli printingFib  (Fix $ SuccF $ Fix $ SuccF $ Fix ZeroF)
-- (1,1)
-- (1,2)
-- (2,3)
-- ghci>
-- (those are what's printed out, the actual result would be (pure (2,3)) )

data StreamF e a = ConsF e a
    deriving (Show, Functor)

type Stream e = Fix (StreamF e)

instance Foldable (StreamF e) where
    foldr f z (ConsF e a) = f a z

instance Traversable (StreamF e) where
    traverse g (ConsF e a) = fmap (ConsF e) (g a)

streamToList :: Fix (StreamF e) -> [e]
streamToList (Fix (ConsF e es)) = e : streamToList es

type Coalgebra f a = a -> f a

{- anamorphism on coalgebra

              fmap (ana coalg)
    f (Fix f) <--------- f a
       |                  ^
       |Fix               |coalg
       v     ana coalg    |
     Fix f <------------  a
-}

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

cofib :: Integral a => Coalgebra (StreamF a) (a, a)
cofib (n,m) = ConsF n (m, n+m)

{- anamorphism on coalgebra, in a Kleisli category

              f (ana coalg)
    f (Fix f) <--------- f a
       |                  ^
       |Fix               |coalg :: a -> m (f a)
       v     ana coalg    |
     Fix f <------------  a
-}

type KleisliCoalgebra m f a = a -> m (f a)

anaKleisli :: forall m f a. (Monad m, Traversable f) => KleisliCoalgebra m f a -> a -> m (Fix f)
anaKleisli coalgK = (return . Fix) <=< traverse (anaKleisli coalgK) <=< coalgK

printingCoalg :: (Functor f, Show a) => Coalgebra f a -> KleisliCoalgebra IO f a
printingCoalg coalg a = let fa = coalg a in print a >> return fa

-- prints forever, even if you do e.g.
-- (take 30 . streamToList) <$> anaKleisli printingCofib (0, 1)
-- because it wants to print the entire (infinite) stream, then return the first 30 elements
printingCofib = printingCoalg cofib


filterStream p (Fix (ConsF x xs)) = let tail = filterStream p xs
                                        in if p x then Fix (ConsF x tail)
                                                  else tail

streamEnumFrom :: Enum e => e -> Stream e
streamEnumFrom e = Fix (ConsF e (streamEnumFrom (succ e)))

era :: Coalgebra (StreamF Int) (Stream Int)
era (Fix (ConsF n ns)) = ConsF n (filterStream ((/= 0) . (`mod` n)) ns)
