{-# LANGUAGE DataKinds, TypeFamilies, DerivingVia, RankNTypes, BangPatterns, TypeApplications,
   StandaloneKindSignatures, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module TryingSingletons where

import Data.List
import Data.Int
import qualified Data.Set as Set
import System.IO
import GHC.Generics
import Data.Monoid
import Data.Semigroup
import Data.Char
import Data.Coerce
import qualified Data.Text.Lazy as Text
import Data.Singletons
import Data.Singletons.TH
import Wc

$(genSingletons [''CountMode])

-- TODO make CountBy from a data family into a single CountMode-indexed GADT
-- $(genPromotions [''CountBy])

-- TODO is it Sing [CountMode], 
-- or is it SList SCountMode, or SList CountMode, or ...

foo :: (CountModeC (ListToPairs ms)) => Sing ms -> Text.Text -> (Result (ListToPairs ms))
foo = undefined

type ListToPairs :: [*] -> *
type family ListToPairs as where
    ListToPairs '[] = ()
    ListToPairs (t ': ts) = (t, ListToPairs ts)

type family PairsToList p where
    PairsToList () = '[]
    PairsToList (a, b) = a ': (PairsToList b)
