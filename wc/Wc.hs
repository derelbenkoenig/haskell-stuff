{-# LANGUAGE DataKinds, TypeFamilies, DerivingVia, RankNTypes
    , BangPatterns, TypeApplications, ScopedTypeVariables, FlexibleInstances #-}

module Wc where

import Control.Monad ( (>=>) )
import qualified Data.ByteString.Lazy as BS
import Data.Binary (encode)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import Data.List
import Data.Int
import qualified Data.Set as Set
import System.IO
import Data.Monoid
import Data.Semigroup
import Data.Proxy
import Data.Char
import Data.Coerce

data CountMode = Lines | Words | Chars | Bytes | MaxLineLength
    deriving (Eq, Ord, Enum, Show)

class Monoid m => CountModeC m where
    type Result m :: *
    fromChar :: Char -> m
    getResult :: m -> (Result m)

instance CountModeC () where
    type Result () = ()
    fromChar _ = ()
    getResult _ = ()

instance (CountModeC m1, CountModeC m2) => CountModeC (m1, m2) where
    type Result (m1, m2) = (Result m1, Result m2)
    fromChar !c = (fromChar c, fromChar c)
    getResult (!x,!y) = (getResult x, getResult y)

data family CountBy :: CountMode -> *

newtype instance CountBy Lines = CountLines Int64
    deriving (Show)
    deriving (Semigroup, Monoid) via (Sum Int64)

instance CountModeC (CountBy Lines) where
    type Result (CountBy Lines) = Int64
    fromChar c = CountLines $ if c == '\n' then 1 else 0
    getResult = coerce

data instance CountBy Words = CountWords
    { whitespaceLeft :: {-# UNPACK #-} !Bool,
      currentCount :: {-# UNPACK #-} !Int64,
      whitespaceRight :: {-# UNPACK #-} !Bool
    } | CountWordsEmpty
    deriving (Show)

instance Semigroup (CountBy Words) where
    CountWordsEmpty <> x = x
    x <> CountWordsEmpty = x
    CountWords l c False <> CountWords False c' r' = CountWords l (c + c' - 1) r'
    CountWords l c _ <> CountWords _ c' r' = CountWords l (c + c') r'

instance Monoid (CountBy Words) where
    mempty = CountWordsEmpty

instance CountModeC (CountBy Words) where
    type Result (CountBy Words) = Int64
    fromChar c = if isSpace c
                               then CountWords True 0 True
                               else CountWords False 1 False
    getResult CountWordsEmpty = 0
    getResult (CountWords _ n _) = n

newtype instance CountBy Chars = CountChars Int64
    deriving (Show)
    deriving (Semigroup, Monoid) via (Sum Int64)

instance CountModeC (CountBy Chars) where
    type Result (CountBy Chars) = Int64
    -- TODO figure out how to do chars and bytes differently
    -- _probably_ do it on ByteStrings, which means we have the bytes directly
    -- and have to get chars from that. 
    fromChar _ = CountChars 1
    getResult = coerce

newtype instance CountBy Bytes = CountBytes Int64
    deriving (Show)
    deriving (Semigroup, Monoid) via (Sum Int64)

instance CountModeC (CountBy Bytes) where
    type Result (CountBy Bytes) = Int64
    -- TODO figure out how to do chars and bytes differently
    -- _probably_ do it on ByteStrings, which means we have the bytes directly
    -- and have to get chars from that. 
    fromChar _ = CountBytes 1
    getResult = coerce

data instance CountBy MaxLineLength =
    CountLineLengthUnbroken 
        { lineLength :: {-# UNPACK #-} !Int64 
        } |
    CountMaxLineLength
        { lengthLeft :: {-# UNPACK #-} !Int64,
          knownMax :: {-# UNPACK #-} !Int64,
          lengthRight :: {-# UNPACK #-} !Int64
        }
    deriving (Show)

instance Semigroup (CountBy MaxLineLength) where
    CountLineLengthUnbroken x <> CountLineLengthUnbroken x' =
        CountLineLengthUnbroken (x + x')
    CountLineLengthUnbroken x <> CountMaxLineLength l o r = 
        let combined = l + x 
            in CountMaxLineLength combined (max o combined) r
    CountMaxLineLength l o r <> CountLineLengthUnbroken x =
        let combined = r + x
            in CountMaxLineLength l (max o combined) combined
    CountMaxLineLength l o r <> CountMaxLineLength l' o' r' =
        let combined = r + l'
            in CountMaxLineLength l (max o $ max o' combined) r'

instance Monoid (CountBy MaxLineLength) where
    mempty = CountLineLengthUnbroken 0

instance CountModeC (CountBy MaxLineLength) where
    type Result (CountBy MaxLineLength) = Int64
    fromChar c = if c == '\n'
                     then CountMaxLineLength 0 0 0
                     else CountLineLengthUnbroken 1
    getResult (CountLineLengthUnbroken n) = n
    getResult (CountMaxLineLength _ n _) = n

testStr = "abcdef\nab\nab cd\n"

totals :: [[Int64]] -> [Int64]
totals = foldl' (zipWith (+)) (repeat 0)

isLengthAtLeast n xs = if n <= 0 then True else go xs where
    go [] = False
    go (_:xs') = isLengthAtLeast (n - 1) xs'

fold' :: (Foldable t, Monoid a) => t a -> a
fold' = foldl' (<>) mempty

doCountText :: CountModeC m => Proxy m -> Text.Text -> (Result m)
doCountText (Proxy :: Proxy m) = 
    (getResult :: m -> Result m)
    . Text.foldl' (\a c -> a <> (fromChar :: Char -> m) c) mempty

countBy :: CountMode -> Text.Text -> Int64
countBy mode = case mode of
    Lines -> doCountText (Proxy @(CountBy Lines))
    Words -> doCountText (Proxy @(CountBy Words))
    Chars -> doCountText (Proxy @(CountBy Chars))
    Bytes -> doCountText (Proxy @(CountBy Bytes))
    MaxLineLength -> doCountText (Proxy @(CountBy MaxLineLength))
