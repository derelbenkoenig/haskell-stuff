{-# OPTIONS_GHC -Wno-x-partial #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Foo where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Kind
import Data.Functor.Const
import Data.Monoid
import Data.Functor.Contravariant

newtype ReadedT r m a = ReadedT { runReadedT :: m (r -> a) }

instance Functor m => Functor (ReadedT r m) where
    fmap f (ReadedT m) = ReadedT $ fmap (f .) m

instance Applicative m => Applicative (ReadedT r m) where
    pure a = ReadedT $ pure (const a)
    ReadedT f <*> ReadedT a = ReadedT $ (<*>) <$> f <*> a

-- | cofmap does the reverse of fmap
--   This seems to apply to structures in which it is possible to put a single element in,
--   apply the function, and pull the single element back out.
class Cofunctor f where
    type Suitable f a :: Constraint
    type Suitable f a = ()
    cofmap :: (Suitable f a, Suitable f b) => (f a -> f b) -> a -> b

data Dict c where
    Dict :: c => Dict c

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
    extend :: (w a -> b) -> w a -> w b
    extend wab = fmap wab . duplicate
    {-# MINIMAL extract, (duplicate | extend) #-}

data Env e a = Env e a
    deriving Functor

instance Cofunctor (Env e) where
    cofmap f a = extract $ f $ Env undefined a

instance Comonad (Env e) where
    extract (Env _ a) = a
    duplicate (Env e a) = Env e (Env e a)
    extend wab eea@(Env e _) = Env e (wab eea)

instance Cofunctor Set where
    type Suitable Set a = Ord a
    cofmap f = head . Set.toAscList . f . Set.singleton

(<^>) :: (e -> a -> b) -> (e -> a) -> e -> b
(<^>) = (<*>)

pointfulDistribRProduct, distribRProduct, curryDistribRProduct
    :: (Either b c, a) -> Either (b,a) (c,a)

pointfulDistribRProduct ebca =
    either (\x -> Left (x, snd ebca)) (\x -> Right (x, snd ebca)) (fst ebca)

distribRProduct = uncurry $ either ((Left .) . (,)) ((Right .) . (,))

curryDistribRProduct = uncurry $ either (curry Left) (curry Right)

curryDistribLProduct
    :: (a, Either b c) -> Either (a, b) (a, c)
curryDistribLProduct = uncurry $ flip $ either (flip $ curry Left) (flip $ curry Right)

class Bitraversable t where
    bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')

instance Bitraversable (,) where
    bitraverse f g (x,y) = (,) <$> f x <*> g y

both :: (Bitraversable t, Applicative f) => (b -> f b') -> t b b -> f (t b' b')
both f = bitraverse f f

folded :: (Foldable t, Applicative f, Contravariant f) => (a -> f b) -> t a -> f s
folded = foldring foldr

foldring
    :: (Contravariant f, Applicative f)
    => ((a -> f a -> f a) -> f a -> s -> f a)
    -> (a -> f b) -> s -> f t
foldring localFoldr f = phantom . localFoldr ((<*>) . phantom . f) (phantom (pure ()))

foldMapOf
    :: Monoid m => ((a -> Const (Endo [a]) b) -> s -> Const (Endo [a]) t) -> (a -> m) -> s -> m
foldMapOf l f = foldMap f . flip appEndo [] . getConst . l (Const . Endo . (:))

foldMapOf'
    :: ((a -> Const m b) -> s -> Const m t) -> (a -> m) -> s -> m
foldMapOf' l f = getConst . l (Const . f)

foldOf
    :: Monoid a => ((a -> Const (Endo a) b) -> s -> Const (Endo a) t) -> s -> a
foldOf l = flip appEndo mempty . getConst . l (Const . Endo . (<>))
