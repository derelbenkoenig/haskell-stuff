{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
module Anagram where

import Data.Char (isAlpha, toLower)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Trie (Trie)
import qualified Data.Trie as Trie

import System.IO

loadWordSet :: FilePath -> IO (Trie BS.ByteString)
loadWordSet fp = openFile fp ReadMode >>= go Trie.empty where
    go acc h = do
        eof <- hIsEOF h
        if eof
            then return acc
            else do
                ln <- clean <$> BS.hGetLine h
                go (Trie.insert (BS.sort ln) ln acc) h

clean :: ByteString -> ByteString
clean = BS.map toLower . BS.filter isAlpha

findAnagramsWith :: Trie ByteString -> ByteString -> [ByteString]
findAnagramsWith wordSet = lowerCodensity . go where
    go w =
        if BS.null w
            then pure BS.empty
            else do
                trieKey <- liftCodensity $ tail $ BS.inits $ BS.sort w
                undefined

newtype Codensity m a =
    Codensity { runCodensity :: forall r. (a -> m r) -> m r }

instance Functor (Codensity m) where
    fmap f (Codensity k) = Codensity $ \g -> k (g . f)

instance Applicative (Codensity m) where
    pure a = Codensity (\f -> f a)
    Codensity f <*> Codensity g = Codensity $ \h -> f (\k -> g (h . k))

instance Monad (Codensity m) where
    return = pure
    Codensity f >>= g = Codensity $ \h -> f $ \a -> runCodensity (g a) h

liftCodensity :: Monad f => f a -> Codensity f a
liftCodensity x = Codensity $ \k -> x >>= k

lowerCodensity :: Monad f => Codensity f a -> f a
lowerCodensity (Codensity k) = k pure
