{-# LANGUAGE
   BangPatterns,
   DataKinds,
   FlexibleInstances,
   FunctionalDependencies,
   GADTs,
   RankNTypes,
   ScopedTypeVariables,
   TypeApplications,
   TypeFamilies,
   UndecidableInstances
   #-}

module Wc where

import Data.List
import Data.Int
import Data.Char

--
-- the CountModeC class
--

class Monoid m => CountModeC r m | m -> r where
    fromChar :: Char -> m
    getResult :: m -> r

instance CountModeC () () where
    fromChar _ = ()
    getResult _ = ()

instance (CountModeC r1 m1, CountModeC r2 m2) => CountModeC (r1, r2) (m1, m2) where
    fromChar !c = (fromChar c, fromChar c)
    getResult (!x,!y) = (getResult x, getResult y)

--
-- The CountMode enum
--

data CountMode = Lines | Words | Chars | Bytes | MaxLineLength
    deriving (Eq, Ord, Enum, Show)

--
-- The CountBy data family
--

data CountBy (m :: CountMode) where
    CountLines :: !Int64 -> CountBy Lines
    CountWords :: { whitespaceLeft :: !Bool,
                    currentCount :: !Int64,
                    whitespaceRight :: !Bool } -> CountBy Words
    CountWordsEmpty :: CountBy Words
    CountChars :: !Int64 -> CountBy Chars
    CountBytes :: !Int64 -> CountBy Bytes
    CountLineLengthUnbroken :: { lineLength :: !Int64 } -> CountBy MaxLineLength
    CountMaxLineLength :: { lengthLeft :: !Int64,
                            knownMax :: !Int64,
                            lengthRight :: !Int64 } -> CountBy MaxLineLength

-- instances for lines
--
instance Semigroup (CountBy Lines) where
    CountLines x <> CountLines y = CountLines (x + y)
instance Monoid (CountBy Lines) where
    mempty = CountLines 0

instance CountModeC Int64 (CountBy Lines) where
    fromChar c = CountLines $ if c == '\n' then 1 else 0
    getResult (CountLines x) = x

-- instances for words
--
instance Semigroup (CountBy Words) where
    CountWordsEmpty <> x = x
    x <> CountWordsEmpty = x
    CountWords l c False <> CountWords False c' r' = CountWords l (c + c' - 1) r'
    CountWords l c _ <> CountWords _ c' r' = CountWords l (c + c') r'

instance Monoid (CountBy Words) where
    mempty = CountWordsEmpty

instance CountModeC Int64 (CountBy Words) where
    fromChar c = if isSpace c
                               then CountWords True 0 True
                               else CountWords False 1 False
    getResult CountWordsEmpty = 0
    getResult (CountWords _ n _) = n

-- instances for chars
--
instance Semigroup (CountBy Chars) where
    CountChars x <> CountChars y = CountChars (x + y)
instance Monoid (CountBy Chars) where
    mempty = CountChars 0

instance CountModeC Int64 (CountBy Chars) where
    -- TODO figure out how to do chars and bytes differently
    -- _probably_ do it on ByteStrings, which means we have the bytes directly
    -- and have to get chars from that. 
    fromChar _ = CountChars 1
    getResult (CountChars x) = x

-- instances for bytes
--
instance Semigroup (CountBy Bytes) where
    CountBytes x <> CountBytes y = CountBytes (x + y)
instance Monoid (CountBy Bytes) where
    mempty = CountBytes 0

instance CountModeC Int64 (CountBy Bytes) where
    -- TODO figure out how to do chars and bytes differently
    -- _probably_ do it on ByteStrings, which means we have the bytes directly
    -- and have to get chars from that. 
    fromChar _ = CountBytes 1
    getResult (CountBytes x) = x

-- instances for maxLineLength
--
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

instance CountModeC Int64 (CountBy MaxLineLength) where
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
