{-# LANGUAGE DataKinds, TypeFamilies, DerivingVia #-}

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
import GHC.Generics
import Data.Monoid
import Data.Semigroup

countFileBy :: CountMode -> Handle -> IO Int64
countFileBy f = fmap (countFunc f) . TextIO.hGetContents

data CountMode = Lines | Words | Chars | Bytes | MaxLineLength
    deriving (Eq, Ord, Enum, Show, Generic)

data family CountBy :: CountMode -> *

newtype instance CountBy Lines = CountLines Int64
    deriving (Show, Generic)
    deriving (Semigroup, Monoid) via (Sum Int64)

data instance CountBy Words = CountWords
    { whitespaceLeft :: {-# UNPACK #-} !Bool,
      currentCount :: {-# UNPACK #-} !Int64,
      whitespaceRight :: {-# UNPACK #-} !Bool
    } | CountWordsEmpty
    deriving (Show, Generic)

instance Semigroup (CountBy Words) where
    CountWordsEmpty <> x = x
    x <> CountWordsEmpty = x
    CountWords l c False <> CountWords False c' r' = CountWords l (c + c' - 1) r'
    CountWords l c _ <> CountWords _ c' r' = CountWords l (c + c') r'

instance Monoid (CountBy Words) where
    mempty = CountWordsEmpty

newtype instance CountBy Chars = CountChars Int64
    deriving (Show, Generic)
    deriving (Semigroup, Monoid) via (Sum Int64)

newtype instance CountBy Bytes = CountBytes Int64
    deriving (Show, Generic)
    deriving (Semigroup, Monoid) via (Sum Int64)

data instance CountBy MaxLineLength =
    CountLineLengthUnbroken 
        { lineLength :: {-# UNPACK #-} !Int64 
        } |
    CountMaxLineLength
        { lengthLeft :: {-# UNPACK #-} !Int64,
          knownMax :: {-# UNPACK #-} !Int64,
          lengthRight :: {-# UNPACK #-} !Int64
        }
    deriving (Show, Generic)

maxLineLengthFromChar c = if c == '\n'
                              then CountMaxLineLength 0 0 0
                              else CountLineLengthUnbroken 1

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

-- TODO remove
countFunc :: CountMode -> Text.Text -> Int64
countFunc mode = case mode of
        Lines -> genericLength . Text.lines
        Words -> genericLength . Text.words
        Chars -> Text.length
        Bytes -> BS.length . encode
        MaxLineLength -> maximum . (map Text.length) . Text.lines

-- TODO remove
getCounts :: Set.Set CountMode -> Handle -> IO [Int64]
getCounts modes h = (\t -> map (($ t) . countFunc) (Set.toAscList modes)) <$> TextIO.hGetContents h

totals :: [[Int64]] -> [Int64]
totals = foldl' (zipWith (+)) (repeat 0)

isLengthAtLeast n xs = if n <= 0 then True else go xs where
    go [] = False
    go (_:xs') = isLengthAtLeast (n - 1) xs'

fold' :: (Foldable t, Monoid a) => t a -> a
fold' = foldl' (<>) mempty

