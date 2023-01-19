{-# LANGUAGE LambdaCase #-}

module ContThings where

import Data.Functor ((<&>))
import Control.Monad.Cont
import Control.Monad.Trans

newtype Coroutine s m r = Coroutine { resume :: m (CoroutineState s m r) }

data CoroutineState s m r = Run (s (Coroutine s m r)) | Done r

instance (Functor s, Functor m) => Functor (Coroutine s m) where
    fmap f (Coroutine m) = Coroutine $ m <&> \case
        Done r -> Done $ f r
        Run s -> Run $ (fmap . fmap) f s

instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
    pure a = Coroutine $ pure $ Done a
    f <*> a = undefined

instance (Functor s, Monad m) => Monad (Coroutine s m) where
    return = pure
    c >>= f = undefined

instance Functor s => MonadTrans (Coroutine s) where
    lift = undefined

suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Run s))

data Interface i o x = Produced o (i -> x)

instance Functor (Interface i o) where
    fmap f (Produced o g) = Produced o (f . g)

type Producing o i = Coroutine (Interface i o)
type Consuming r m i o = i -> Producing o i m r

yield :: forall i o m. (Monad m) => o -> Producing o i m i
yield o =
    let 
        iof :: Interface i o (Coroutine (Interface i o) m i)
        iof = Produced o f
        f :: i -> Coroutine (Interface i o) m i
        f i = Coroutine $ return $ Done i
        in suspend iof

($$) :: Monad m => Producing a b m r -> Consuming r m a b -> m r
producing $$ consuming = undefined
        

