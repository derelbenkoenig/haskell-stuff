{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
module Middleware where

import Control.Monad
import Data.Bifunctor
import Data.Constraint

newtype Middleware a b c m d =
    Middleware { wrapWithMW :: (b -> m c) -> a -> m d }

instance Functor m => Functor (Middleware a b c m) where
    fmap :: (d -> d') -> Middleware a b c m d -> Middleware a b c m d'
    fmap f m = Middleware $ \ inner -> fmap f . wrapWithMW m inner

instance Applicative m => Applicative (Middleware a b c m) where
    pure :: d -> Middleware a b c m d
    pure d = Middleware $ \ _ _ -> pure d

    (<*>) :: Middleware a b c m (d -> d') -> Middleware a b c m d -> Middleware a b c m d'
    -- Middleware wrapF <*> Middleware wrapD = Middleware $ \ inner a ->
    --     wrapF inner a <*> wrapD inner a
    f <*> x = Middleware $ \ inner a -> wrapWithMW f inner a <*> wrapWithMW x inner a

instance Monad m => Monad (Middleware a b c m) where
    (>>=) :: Middleware a b c m d -> (d -> Middleware a b c m d') -> Middleware a b c m d'
    m >>= f = Middleware $ \ inner a ->
        let wrapped = wrapWithMW m inner
            next d = wrapWithMW (f d) inner a
         in wrapped a >>= next

instance MonadTrans (Middleware a b c) where
    lift :: m d -> Middleware a b c m d
    lift md = Middleware $ \ _ _ -> md
    transformsMonad = implied

type Middleware' a m b = Middleware a a b m b

middleware :: Monad m => (a -> m b) -> (a -> c -> m d) -> Middleware a b c m d
middleware before after = Middleware $ \ inner a -> before a >>= inner >>= after a

middleware_ :: Applicative m => (a -> m ()) -> (a -> m ()) -> Middleware' a m b
middleware_ before after = Middleware $ \ inner a -> before a *> inner a <* after a

preprocess :: Monad m => (a -> m b) -> Middleware a b c m c
preprocess f = Middleware (f >=>)

postprocess :: Monad m => (b -> m c) -> Middleware a a b m c
postprocess f = Middleware (f <=<)

around
    :: Middleware a b c m d
    -> Middleware b b' c' m c
    -> Middleware a b' c' m d
around outer inner = Middleware $ \ f a -> wrapWithMW outer (wrapWithMW inner f) a

incrAfter :: MonadState Int m => Middleware' a m b
incrAfter = middleware pure $ \ _ b -> modify (+ 1) >> pure b
-- incrAfter = Middleware $ \ f a -> f a >>= \ b -> modify (+ 1) >> pure b

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

instance Functor m => Functor (StateT s m) where
    fmap f m = StateT $ fmap (second f) . runStateT m

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \ s -> pure (s, a)
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    m >>= f = StateT $ \ s -> runStateT m s >>= \ (s', b) -> runStateT (f b) s'

class Monad m => MonadState s m | m -> s where
    {-# MINIMAL state | (get, put) #-}
    state :: (s -> (s, a)) -> m a
    state f = do
        s <- get
        let (s', a) = f s
        put s'
        pure a

    get :: m s
    get = state $ \s -> (s,s)

    put :: s -> m ()
    put s = state $ \_ -> (s, ())

instance Monad m => MonadState s (StateT s m) where
    state f = StateT $ pure . f

class MonadTrans t where
    lift :: forall m a. Monad m => m a -> t m a

    transformsMonad :: Monad m :- Monad (t m)

instance MonadTrans (StateT s) where
    lift m = StateT $ \ s -> (s,) <$> m
    transformsMonad = implied

modify :: MonadState s m => (s -> s) -> m ()
modify f = state $ (,()) . f
