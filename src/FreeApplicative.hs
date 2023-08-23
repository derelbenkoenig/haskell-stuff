{-# LANGUAGE GADTSyntax #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module FreeApplicative where

import Control.Monad (ap)

data Free f a where
    Return :: a -> Free f a
    Free :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
    fmap h (Return a) = Return (h a)
    fmap h (Free f) = Free (fmap (fmap h) f)

instance Functor f => Applicative (Free f) where
    pure = Return
    (<*>) = ap

instance Functor f => Monad (Free f) where
    return = pure
    m >>= f = joinFree (fmap f m)

joinFree :: Functor f => Free f (Free f a) -> Free f a
joinFree (Return m1) = m1
joinFree (Free m1) = Free (fmap joinFree m1)

lowerFree :: Monad m => Free m a -> m a
lowerFree (Return a) = return a
lowerFree (Free m) = m >>= lowerFree

data FreeA f a where
    Pure :: a -> FreeA f a
    (:$:) :: f (b -> a) -> FreeA f b -> FreeA f a

instance Functor f => Functor (FreeA f) where
    fmap h (Pure a) = Pure (h a)
    fmap h (f :$: a) = fmap (h .) f :$: a

instance Functor f => Applicative (FreeA f) where
    pure = Pure
    Pure h <*> a = fmap h a
    (h :$: x) <*> y = fmap (flip id) h :$: (churchPair <$> x <*> y) where
        churchPair e1 e2 cont = cont e1 e2

type Nat f g = forall a. f a -> g a

type AppNat f g = forall a. f a -> g a

liftA2M :: Functor f => FreeA f a -> Free f a
liftA2M (Pure a) = Return a
liftA2M (f :$: a) = Free (fmap (\h -> fmap h (liftA2M a)) f)

liftT :: (Functor f, Functor g) => Nat f g -> AppNat (FreeA f) (FreeA g)
liftT _ (Pure a) = pure a
liftT k (f :$: a) = k f :$: liftT k a

raise :: (Functor f, Applicative g) => Nat f g -> AppNat (FreeA f) g
raise _ (Pure a) = pure a
raise k (f :$: a) = k f <*> raise k a

one :: Functor f => Nat f (FreeA f)
one = (:$: Pure ()) . fmap const

lower :: (Functor f, Applicative g) => AppNat (FreeA f) g -> Nat f g
lower k = k . one
