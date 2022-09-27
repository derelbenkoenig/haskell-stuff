{-# LANGUAGE
   AllowAmbiguousTypes,
   ConstraintKinds,
   DataKinds,
   FlexibleContexts,
   FlexibleInstances,
   GADTs,
   MultiParamTypeClasses,
   PartialTypeSignatures,
   QuantifiedConstraints,
   RankNTypes,
   ScopedTypeVariables,
   StandaloneDeriving,
   StandaloneKindSignatures,
   TemplateHaskell,
   TypeApplications,
   TypeFamilies,
   TypeInType,
   TypeOperators,
   UndecidableInstances
   #-}

module TryingSingletons where

import Data.Singletons
import Data.Singletons.TH
import Data.List.Singletons
import Text.Show.Singletons
import Data.Kind
import Data.List (foldl')
import Data.Int
import Exinst

import Wc

$(genSingletons [''CountMode])
$(showSingInstances [''CountMode])

instance (c (f Words),
          c (f Lines),
          c (f Chars),
          c (f Bytes),
          c (f MaxLineLength)) => Dict1 c f where
              dict1 x = case x of
                  SWords -> Dict
                  SLines -> Dict
                  SChars -> Dict
                  SBytes -> Dict
                  SMaxLineLength -> Dict

class (Result a ~ Int64) => CountsInt64 a

-- I'll change this to operate on ByteString or something later...
foo :: forall (ms :: [CountMode]) r.
    (CountModeC (CountByModes ms),
    PairList (Result (CountByModes ms)) r) =>
    Sing ms -> String -> [r]
foo modes = toList . getResult @(CountByModes ms) .
    foldl' (\count char -> count <> fromChar @(CountByModes ms) char) mempty

foo' :: [CountMode] -> String -> [Int64]
foo' modes = case toSing modes of
    SomeSing sms -> undefined -- foo sms

bar :: forall (m :: CountMode). (CountModeC (CountBy m)) => Sing m -> String -> Result (CountBy m)
bar m = getResult . foldl' (\count char -> count <> fromChar @(CountBy m) char) mempty

bar' :: CountMode -> String -> Int64
bar' m = withSomeSing m $ \(sm :: Sing a) ->
    case (dict1 sm :: Dict (CountModeC (CountBy a))) of
        Dict -> case (dict1 sm :: Dict (CountsInt64 (CountBy a))) of
            Dict -> bar sm

type CountByModes :: [CountMode] -> *
type family CountByModes (ms :: [CountMode]) where
    CountByModes '[] = ()
    CountByModes (m:ms) = (CountBy m, CountByModes ms)

type ListToPairs :: [*] -> *
type family ListToPairs as where
    ListToPairs '[] = ()
    ListToPairs (t ': ts) = (t, ListToPairs ts)

class PairList p e where
    toList :: p -> [e]

instance PairList () e where
    toList () = []

instance PairList es e => PairList (e, es) e where
    toList (x, xs) = x : toList xs
