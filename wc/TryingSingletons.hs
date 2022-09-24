{-# LANGUAGE
   DataKinds,
   FlexibleContexts,
   FlexibleInstances,
   GADTs,
   MultiParamTypeClasses,
   PartialTypeSignatures,
   RankNTypes,
   ScopedTypeVariables,
   StandaloneDeriving,
   StandaloneKindSignatures,
   TemplateHaskell,
   TypeApplications,
   TypeFamilies,
   TypeOperators,
   UndecidableInstances
   #-}

module TryingSingletons where

import Data.Singletons
import Data.Singletons.TH
import Data.List.Singletons
import Text.Show.Singletons
import Data.List (foldl')
import Data.Int

import Wc

$(genSingletons [''CountMode])
$(showSingInstances [''CountMode])

-- I'll change this to operate on ByteString or something later...
foo :: forall (ms :: [CountMode]) r.
    (CountModeC (CountByModes ms),
    PairList (Result (CountByModes ms)) r) =>
    Sing ms -> String -> [r]
foo modes = toList . getResult @(CountByModes ms) .
    foldl' (\count char -> count <> fromChar @(CountByModes ms) char) mempty

foo' :: [CountMode] -> String -> [Int64]
foo' modes = case toSing modes of
    SomeSing sms -> undefined

bar :: forall (m :: CountMode). (CountModeC (CountBy m)) => Sing m -> String -> Result (CountBy m)
bar m = getResult . foldl' (\count char -> count <> fromChar @(CountBy m) char) mempty

bar' :: CountMode -> String -> Int64
bar' m = undefined --withSomeSing m bar

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
