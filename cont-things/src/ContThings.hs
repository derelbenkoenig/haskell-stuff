{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ContThings where

import Data.Monoid (Endo(..))
import Control.Applicative

newtype Yoneda f a = Yoneda { runYoneda :: forall r. (a -> r) -> f r }

instance Functor (Yoneda f) where
    fmap f (Yoneda g) = Yoneda $ \h -> g (h . f)

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda fa = Yoneda $ \g -> fmap g fa

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda g) = g id

newtype YonedaEndo a = YonedaEndo { getYonedaEndo :: Yoneda Endo a }

runYonedaEndo :: YonedaEndo a -> forall r. (a -> r) -> r -> r
runYonedaEndo = (\f k r -> appEndo (f k) r) . runYoneda . getYonedaEndo

instance Functor YonedaEndo where
    -- more involved than necessary; we could leave out the Endo part
    -- entirely, i.e.: \h -> g (h . f)
    -- but if you want the Endo part to be visible...
    fmap f (YonedaEndo (Yoneda g)) =
        YonedaEndo $ Yoneda $
            \h -> Endo $ \r -> appEndo (g (h . f)) r

instance Applicative YonedaEndo where
    pure a = YonedaEndo $ Yoneda $ \h -> Endo $ \_ -> h a
    YonedaEndo (Yoneda f) <*> YonedaEndo (Yoneda g) =
        YonedaEndo $ Yoneda $
            \h -> Endo $ \r -> appEndo (f (\k -> appEndo (g (h . k)) r)) r

instance Alternative YonedaEndo where
    empty = YonedaEndo $ Yoneda $ \_ -> Endo id
    YonedaEndo (Yoneda f) <|> YonedaEndo (Yoneda g) =
        YonedaEndo $ Yoneda $
            \h -> Endo $ \r -> appEndo (f h) (appEndo (g h) r)

instance Monad YonedaEndo where
    return = pure
    (YonedaEndo (Yoneda f)) >>= g =
        YonedaEndo $ Yoneda $
            \h -> Endo $ \r -> appEndo (f (\a -> runYonedaEndo (g a) h r)) r

-- I wish join and (>>=) were both methods of Monad and you could implement
-- either for a minimal definition. But instead join is just a function
-- implemented in terms of (>>=). But, I would like to implement join anyway
-- just for the exercise
joinYonedaEndo :: YonedaEndo (YonedaEndo a) -> YonedaEndo a
joinYonedaEndo (YonedaEndo (Yoneda f)) =
    YonedaEndo $ Yoneda $ \g -> Endo $ \r ->
        appEndo (f (\(YonedaEndo (Yoneda h)) -> appEndo (h g) r)) r

-- Yoneda Endo ~~ Maybe
yonedaEndoToMaybe :: YonedaEndo a -> Maybe a
yonedaEndoToMaybe (YonedaEndo (Yoneda f)) = appEndo (f Just) Nothing

yonedaEndoFromMaybe :: Maybe a -> YonedaEndo a
yonedaEndoFromMaybe Nothing = YonedaEndo $ Yoneda $ \_ -> Endo id
yonedaEndoFromMaybe (Just x) = YonedaEndo $ Yoneda $ \k -> Endo (const (k x))

newtype Codensity f a =
    Codensity { runCodensity :: forall r. (a -> f r) -> f r }

lowerCodensity :: Monad f => Codensity f a -> f a
lowerCodensity (Codensity g) = g return

liftCodensity :: Monad f => f a -> Codensity f a
liftCodensity fa = Codensity (fa >>=)

instance Functor (Codensity f) where
    fmap g (Codensity h) = Codensity $ \k -> h (k . g)

instance Applicative (Codensity f) where
    pure a = Codensity $ \k -> k a
    Codensity f <*> Codensity g = Codensity $ \h ->
        f (\k -> g (h . k))

instance Monad (Codensity f) where
    return = pure
    Codensity f >>= g = Codensity $ \h ->
        f $ \a -> runCodensity (g a) h

newtype CodensityEndo a =
    CodensityEndo { getCodensityEndo :: Codensity Endo a }

runCodensityEndo :: CodensityEndo a -> forall r. (a -> r -> r) -> r -> r
runCodensityEndo =
    (\f k r -> appEndo (f (Endo . k)) r) . runCodensity . getCodensityEndo

-- now for the fun part

instance Functor CodensityEndo where
    fmap f (CodensityEndo (Codensity g)) = CodensityEndo $ Codensity $
        \h -> g (h . f)

instance Applicative CodensityEndo where
    pure a = CodensityEndo $ Codensity $ \k -> k a
    CodensityEndo (Codensity f) <*> CodensityEndo (Codensity g) =
        CodensityEndo $ Codensity $ \h -> Endo $ \r ->
            appEndo (f $ \k -> g (h . k)) r

instance Monad CodensityEndo where
    return = pure
    CodensityEndo (Codensity f) >>= g = CodensityEndo $ Codensity $
        \h -> Endo $ \r ->
            appEndo (f $ \a -> Endo (runCodensityEndo (g a) (appEndo . h))) r

joinCodensityEndo :: CodensityEndo (CodensityEndo a) -> CodensityEndo a
joinCodensityEndo (CodensityEndo (Codensity f)) = CodensityEndo $ Codensity $
    \g -> Endo $ \r -> appEndo (f (\(CodensityEndo (Codensity h)) -> h g)) r

-- CodensityEndo ~~ List
codensityEndoFromList :: [a] -> CodensityEndo a
codensityEndoFromList as = CodensityEndo $ Codensity $ \f -> Endo $ \r ->
    foldr (appEndo . f) r as

codensityEndoToList :: CodensityEndo a -> [a]
codensityEndoToList (CodensityEndo (Codensity f)) =
    appEndo (f (\a -> Endo (a:))) []
