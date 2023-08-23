{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Foo where

import Prelude hiding (
    Maybe(..),
    -- Functor(..),
    -- Applicative(..),
                      )
-- import Data.Kind (Type)

data MyMaybe a = MyNothing | MyJust a

-- class Functor (f :: Type -> Type) where
--     fmap :: (a -> b) -> (f a -> f b)

-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

data Box a = Box a
    deriving Show

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box x) = Box (f x)

data EmptyBox a = EmptyBox

instance Functor EmptyBox where
    fmap _f EmptyBox = EmptyBox

instance Functor MyMaybe where
    fmap _f MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)


data MyList a = MyNil | a :+ MyList a
    deriving (Eq, Show)
infixr 5 :+

instance Functor MyList where
    fmap _f MyNil = MyNil
    fmap f (x :+ xs) = f x :+ fmap f xs

newtype MyFun a b = MyFun (a -> b)

instance Functor (MyFun a) where
    fmap :: (b -> c) -> MyFun a b -> MyFun a c
    fmap f (MyFun g) = MyFun (f . g)
