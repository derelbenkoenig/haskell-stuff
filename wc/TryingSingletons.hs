{-# LANGUAGE DataKinds,
   TypeFamilies,
   DerivingVia,
   RankNTypes,
   BangPatterns,
   TypeApplications,
   StandaloneKindSignatures,
   GADTs,
   UndecidableInstances,
   ScopedTypeVariables,
   TypeOperators,
   FlexibleContexts #-}

{-# LANGUAGE TemplateHaskell #-}

module TryingSingletons where

import Data.Singletons
import Data.Singletons.TH
import Data.List.Singletons
import Text.Show.Singletons
import Data.List (foldl')
import Wc

$(genSingletons [''CountMode])
$(showSingInstances [''CountMode])

$(genDefunSymbols [''CountBy])

foo :: forall ms. (CountModeC (CountByModes ms)) => Sing ms -> String -> (Result (CountByModes ms))
foo modes = getResult @(CountByModes ms) .
    foldl' (\count char -> count <> fromChar @(CountByModes ms) char) mempty

type CountByModes :: [CountMode] -> *
type family CountByModes (m :: [CountMode]) where
    CountByModes ms = ListToPairs (Map (TyCon1 CountBy) ms)

type ListToPairs :: [*] -> *
type family ListToPairs as where
    ListToPairs '[] = ()
    ListToPairs (t ': ts) = (t, ListToPairs ts)
