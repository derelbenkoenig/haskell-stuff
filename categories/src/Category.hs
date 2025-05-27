{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Category where

import qualified Prelude
import Prelude hiding (id, curry)
import Data.Kind (Type)

infixr 9 `o`
class Category k where
    id :: a `k` a
    o :: (b `k` c) -> (a `k` b) -> (a `k` c)

infixr 3 `del`
class Category k => Cartesian k where
    type X k :: Type -> Type -> Type
    del :: (a `k` c) -> (a `k` d) -> (a `k` X k c d)

    exl :: X k a b `k` a
    exr :: X k a b `k` b

class Cartesian k => Closed k where
    type Arr k :: Type -> Type -> Type
    apply :: X k (Arr k a b) a -> b

    curry :: X k a b `k` c -> a `k` Arr k b c
    uncurry :: a `k` Arr k b c -> X k a b `k` c

-- closedUniversal :: Closed k => (X k a b `k` c) `k` (X k a b `k` c)
-- closedUniversal f = apply `o` (Category.curry f `o` exl `del` exr)

-- the universal property states that closedUniversal f === f
-- but, I cannot get it to type check in the general case, only when
-- the category in question is (->) itself
-- and even then, the definition fails to type check without a type application
closedUniversal' :: ((a, b) -> c) -> (a, b) -> c
closedUniversal' f = apply @(->) `o` (curry f `o` exl `del` exr)

instance Category (->) where
    id = Prelude.id
    o = (.)

instance Cartesian (->) where
    type X (->) = (,)
    del f g x = (f x, g x)
    exl = fst
    exr = snd

instance Closed (->) where
    type Arr (->) = (->)
    apply (f,x) = f x
    curry = Prelude.curry
    uncurry = Prelude.uncurry
