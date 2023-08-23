{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module DelayErrors where

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Identity
import Control.Monad.Fix
import Control.Monad

newtype AccumErrorsT e m a = AccumErrorsT { runAccumErrorsT :: m ([e], a) }

type AccumErrors e a = AccumErrorsT e Identity a

instance Functor m => Functor (AccumErrorsT e m) where
    fmap f (AccumErrorsT m) = AccumErrorsT (fmap (second f) m)

instance Applicative m => Applicative (AccumErrorsT e m) where
    pure = AccumErrorsT . pure . ([] ,)
    AccumErrorsT f <*> AccumErrorsT x = AccumErrorsT $
            uncurry bimap . first (++) <$> f <*> x

instance Monad m => Monad (AccumErrorsT e m) where
    AccumErrorsT m >>= f = AccumErrorsT $ do
        (es, a) <- m
        (es', b) <- runAccumErrorsT (f a)
        return (es ++ es', b)

instance Alternative m => Alternative (AccumErrorsT e m) where
    AccumErrorsT m <|> AccumErrorsT w = AccumErrorsT $
        let es = fmap fst m
            es' = fmap fst w
            a = fmap snd m <|> fmap snd w
         in liftA2 (,) (liftA2 (++) es es') a
    empty = AccumErrorsT $ fmap ([], ) empty

newtype Tardis forward backward m a =
    Tardis { runTardis :: forward -> backward -> m (forward, backward, a) }

instance Functor m => Functor (Tardis fw bw m) where
    fmap f (Tardis g) =
        Tardis $ \fo ba -> fmap (\ (a, b, c) -> (a, b, f c)) (g fo ba)

instance MonadFix m => Applicative (Tardis fw bw m) where
    pure a = Tardis $ \fw bw -> pure (fw, bw, a)
    (<*>) = ap

instance MonadFix m => Monad (Tardis fw bw m) where
    Tardis f >>= g = Tardis $ \fw bw -> mdo
        (fw', bw'', a) <- f fw bw'
        (fw'', bw', b) <- runTardis (g a) fw' bw
        return (fw'', bw'', b)

