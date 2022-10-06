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
import Text.Show.Singletons
import Data.Function.Singletons
import Data.List.Singletons

import Data.Constraint
import Exinst

import Data.Kind
import Data.List (foldl')
import Data.Int

import Wc

$(genSingletons [''CountMode])
$(showSingInstances [''CountMode])

type CountByModes :: [CountMode] -> Type
type family CountByModes (ms :: [CountMode]) where
    CountByModes '[] = ()
    CountByModes (m:ms') = (CountBy m, CountByModes ms')

-- this is just totally duplicating the relation expressed in the
-- CountModeC class, which even has a fundep in its definition, so I have
-- a feeling this should be unnecessary, but I would need to get the instance
-- resolution to spit out the respective result type for me...
type family CountModeResult (c :: Type) where
    CountModeResult (CountBy (m :: CountMode)) = Int64
    CountModeResult () = ()
    CountModeResult (a, b) = (CountModeResult a, CountModeResult b)

type family CountModesResult (ms :: [k]) where
    CountModesResult '[] = ()
    CountModesResult (m ': ms') = (Int64, CountModesResult ms')

class PairList e p where
    toList :: p -> [e]

instance PairList e () where
    toList () = []

instance PairList e es => PairList e (e, es) where
    toList (x, xs) = x : toList xs

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

instance (CountModeC r1 a1, CountModeC r2 a2) :=> CountModeC (r1, r2) (a1, a2) where
    ins = Sub Dict

instance PairList e es :=> PairList e (e, es) where
    ins = Sub Dict

countByModesDict :: Sing (ms :: [CountMode]) ->
    Dict (PairList Int64 (CountModesResult ms),
          CountModeC (CountModesResult ms) (CountByModes ms))
countByModesDict sms = case sms of
    SNil -> Dict @(PairList Int64 (), CountModeC () ())
    SCons sm sms' -> countByConsDict sm sms'

countByConsDict :: forall (m :: CountMode) (ms :: [CountMode]).
    Sing m
    -> Sing ms
    -> Dict
          (PairList Int64 (CountModesResult (m:ms)),
           CountModeC (CountModesResult (m:ms)) (CountByModes (m:ms)))
countByConsDict sm sms = let
    headDict = dict1 sm :: Dict (CountModeC Int64 (CountBy m))

    tailDict :: Dict (PairList Int64 (CountModesResult ms),
                      CountModeC (CountModesResult ms) (CountByModes ms))
    tailDict = countByModesDict sms

    pairListEntailment :: (PairList Int64 (CountModesResult ms)) :-
                          (PairList Int64 (CountModesResult (m:ms)))
    pairListEntailment = ins

    countEntailment :: (CountModeC Int64 (CountBy m),
                        CountModeC (CountModesResult ms) (CountByModes ms)) :-
                       (CountModeC (CountModesResult (m:ms)) (CountByModes (m:ms)))
    countEntailment = ins 

    induction = pairListEntailment *** countEntailment

    in Dict \\ induction \\ headDict \\ tailDict

andDict :: Dict a -> Dict b -> Dict (a, b)
andDict da db = Dict \\ da \\ db

-- I'll change this to operate on ByteString or something later...
foo :: forall (ms :: [CountMode]) r rs.
    (CountModeC rs (CountByModes ms),
     PairList r rs) =>
    Sing ms -> String -> [r]
foo modes = toList . getResult @rs @(CountByModes ms) .
    foldl' (\count char -> count <> fromChar @rs @(CountByModes ms) char) mempty

-- TODO I probably need to use a Some1 instance for PairList or something?
foo' :: [CountMode] -> String -> [Int64]
foo' ms = withSomeSing ms $ \(sms :: Sing (ms' :: [CountMode])) ->
    case countByModesDict sms of Dict -> foo sms

bar :: forall (m :: CountMode) r. (CountModeC r (CountBy m)) => Sing m -> String -> r
bar m = getResult . foldl' (\count char -> count <> fromChar @r @(CountBy m) char) mempty

bar' :: CountMode -> String -> Int64
bar' m = withSomeSing m $ \(sm :: Sing a) ->
    withDict (dict1 sm :: Dict (CountModeC Int64 (CountBy a))) (bar sm)
