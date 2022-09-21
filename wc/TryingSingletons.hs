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
   UndecidableInstances #-}

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

$(genDefunSymbols [''CountBy])

foo :: forall ms r. (CountModeC (CountByModes ms), PairList (Result (CountByModes ms)) Int64) =>
    Sing ms -> String -> [Int64]
foo modes = toList . getResult @(CountByModes ms) .
    foldl' (\count char -> count <> fromChar @(CountByModes ms) char) mempty

foo' :: [CountMode] -> String -> [Int64]
foo' modes = case toSing modes of
    SomeSing sms -> undefined

type CountByModes :: [CountMode] -> *
type family CountByModes (m :: [CountMode]) where
    CountByModes ms = ListToPairs (Map (TyCon1 CountBy) ms)

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
