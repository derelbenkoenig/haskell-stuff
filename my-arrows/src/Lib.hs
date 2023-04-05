{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use <=<" #-}
{-# HLINT ignore "Use bimap" #-}
module Lib where
import Data.List (singleton)

class Arrow arr where
    arr :: (a -> b) -> arr a b
    (>>>) :: arr a b -> arr b c -> arr a c
    (&&&) :: arr a b -> arr a c -> arr a (b, c)
    f &&& g = arr (w (,)) >>> f *** g
    (***) :: arr a b -> arr a' b' -> arr (a, a') (b, b')
    f *** g = first f >>> second g
    first :: arr a b -> arr (a, c) (b, c)
    first f = f *** arr id

second :: Arrow arr => arr a b -> arr (c, a) (c, b)
second f = arr swap >>> first f >>> arr swap

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

class Arrow arr => ArrowChoice arr where
    (|||) :: arr a c -> arr b c -> arr (Either a b) c
    f ||| g = f +++ g >>> arr (either id id)
    (+++) :: arr a b -> arr a' b' -> arr (Either a a') (Either b b')
    f +++ g = left f >>> right g
    left :: arr a b -> arr (Either a c) (Either b c)

right :: ArrowChoice arr => arr a b -> arr (Either c a) (Either c b)
right f = arr mirror >>> left f >>> arr mirror

mirror :: Either a b -> Either b a
mirror = either Right Left

infixl 1 >>>
infixl 2 |||
infixl 3 &&&
infixl 3 ***

instance Arrow (->) where
    arr = id
    (>>>) = flip (.)
    (&&&) = φ (,)
    f *** g = \(a, a') -> (f a, g a')
    first f (a, b) = (f a, b)

instance ArrowChoice (->) where
    (|||) = either
    f +++ g = either (Left . f) (Right . g)
    left f = either (Left . f) Right

φ :: (a -> b -> c) -> (e -> a) -> (e -> b) -> e -> c
φ f g h x = f (g x) (h x)

w :: (a -> a -> b) -> a -> b
w f a = f a a

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>>
         arr (const []) ||| (f *** mapA f >>> arr (uncurry (:))) where
    listcase = \case
        [] -> Left ()
        x:xs -> Right (x, xs)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Arrow (Kleisli m) where
    arr = Kleisli . (return .)
    Kleisli f >>> Kleisli g = Kleisli $ f >>> (>>= g)
    Kleisli f &&& Kleisli g = Kleisli $ \a -> (,) <$> f a <*> g a
    Kleisli f *** Kleisli g = Kleisli $ \(a, a') -> (,) <$> f a <*> g a'
    first (Kleisli f) = Kleisli $ \(a, b) -> fmap (, b) (f a)

instance Monad m => ArrowChoice (Kleisli m) where
    Kleisli f ||| Kleisli g = Kleisli $ either f g
    Kleisli f +++ Kleisli g = Kleisli $ either (fmap Left . f) (fmap Right . g)
    left (Kleisli f) = Kleisli $ either (fmap Left . f) (return . Right)

newtype SF a b = SF { runSF :: [a] -> [b] }

instance Arrow SF where
    arr = SF . fmap
    SF f >>> SF g = SF (f >>> g)
    SF f &&& SF g = SF (f &&& g >>> uncurry zip)
    SF f *** SF g = SF $ unzip >>> f *** g >>> uncurry zip
    first (SF f) = SF $ unzip >>> first f >>> uncurry zip

instance ArrowChoice SF where
    left (SF f) = SF $ \as ->
        let fLefts = as >>= \eac -> either (f . singleton) (const []) eac
            merge :: [b] -> [Either a c] -> [Either b c]
            merge _ [] = []
            merge (x:xs) (Left _:ys) = Left x : merge xs ys
            merge xs (Right y:ys) = Right y : merge xs ys
            merge [] (_:_) = error "somehow more elements were left than total"
         in merge fLefts as

delay :: a -> SF a a
delay a = SF $ init . (a :)

