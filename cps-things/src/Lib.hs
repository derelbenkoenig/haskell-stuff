{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( CPS(..)
    ) where

import Data.Kind
import GHC.Generics

type family ProductLike (r :: Type) (as :: [Type]) where
    ProductLike r '[] = r
    ProductLike r (a:as) = a -> ProductLike r as

type family SumLike (r :: Type) (as :: [Type]) where
    SumLike r '[] = r
    SumLike r (a:as) = (a -> r) -> SumLike r as

class CPS (r :: Type) (a :: Type) where
    type CPSType r a :: Type

    toCPS :: a -> CPSType r a
    default toCPS
        :: (Generic a, GCPS r (Rep a), CPSType r a ~ GCPSType r (Rep a))
        => a
        -> CPSType r a
    toCPS x = gToCPS @_ @r (from x :: Rep a y)

instance CPS r (Maybe a) where
    type CPSType r (Maybe a) = r -> (a -> r) -> r
    toCPS Nothing r _ = r
    toCPS (Just a) _ f = f a

class GCPS (r :: Type) (a :: k -> Type) where
    type GCPSType r a :: Type
    gToCPS :: a p -> GCPSType r a

-- instance (Generic a, GCPS r (Rep a)) => CPS r a where
--     type CPSType r a = GCPSType r (Rep a)
--     toCPS x = gToCPS (from x)

instance GCPS r U1 where
    type GCPSType r U1 = (() -> r) -> r
    gToCPS _ = ($ ())

instance GCPS r (K1 i a) where
    type GCPSType r (K1 i a) = (a -> r) -> r
    gToCPS (K1 a) f = f a

type Tensor k = (k -> Type) -> (k -> Type) -> (k -> Type)

data FuncTypeWitness r where
    ReturnType :: FuncTypeWitness r
    (:->:) :: FuncTypeWitness a -> FuncTypeWitness b -> FuncTypeWitness (a -> b)

class Flattens (r :: Type) (p :: Tensor k) (f :: k -> Type) where
    type Flattened r p f :: Type
    flattened :: p f g a -> Flattened r p f

instance FlattensGo r p f acc => Flattens r p f where
    type Flattened r p f = FlattenedGo r p f r
    -- flattened = flattenedGo flattenedWitness

class FlattensGo (r :: Type) (p :: Tensor k) (f :: k -> Type) (acc :: Type) where
    type FlattenedGo r p f acc
    flattenedWitness :: FuncTypeWitness (FlattenedGo r p f acc)
    flattenedGo :: FuncTypeWitness (FlattenedGo r p f acc) -> p g f a -> FlattenedGo r p f acc

-- instance {-# OVERLAPPABLE #-} FlattensGo ((GCPSType r f -> r) -> (GCPSType r g -> r) -> r) r (:+:) (f :+: g) acc where
--     flattenedGo = _

-- type Undefined = TypeError (Text "Undefined")

-- instance {-# OVERLAPPING #-} FlattensGo r (:+:) (f :+: (g :+: h)) acc where
--     flattenedGo = _

type family Flatten (p :: Tensor k) (a :: k -> Type) :: [k -> Type] where
    Flatten p a = FlattenGo '[] p a

type family FlattenGo (as :: [k -> Type]) (p :: Tensor k) (a :: k -> Type) where
    FlattenGo as p (p f (p g h)) = FlattenGo as p (p (p f g) h)
    FlattenGo as p (p f g) = FlattenGo (g:as) p f
    FlattenGo as p f = f:as
