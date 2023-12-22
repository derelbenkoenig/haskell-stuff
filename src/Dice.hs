{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Dice where

import Control.Monad.Random.Class
import Control.Monad (replicateM)
import Data.List (sortOn, sort)
import Data.Ord (Down(..))
import Data.Singletons.TH

$(singletons [d|
    data RollType =
        Constant |
        Count |
        KeepHighest |
        KeepLowest |
        AddRolls RollType RollType |
        SubRolls RollType RollType |
        MulRolls RollType RollType
    |])

deriving instance Eq RollType

$(singDecideInstance ''RollType)

type family RollResult (t :: RollType) where
    RollResult Constant = Int
    RollResult Count = [Int]
    -- (Kept, Dropped)
    RollResult KeepHighest = ([Int], [Int])
    RollResult KeepLowest = ([Int], [Int])
    RollResult (AddRolls t1 t2) = (RollResult t1, RollResult t2)
    RollResult (SubRolls t1 t2) = (RollResult t1, RollResult t2)
    RollResult (MulRolls t1 t2) = (RollResult t1, RollResult t2)

data DiceRoll (t :: RollType) where
    RollConstant :: Int -> DiceRoll Constant
    -- e.g. roll 3d6 = RollCount 3 6
    RollCount :: Int -> Int -> DiceRoll Count
    RollKeepHighest :: Int -> DiceRoll Count -> DiceRoll KeepHighest
    RollKeepLowest :: Int -> DiceRoll Count -> DiceRoll KeepLowest
    RollPlus :: DiceRoll t1 -> DiceRoll t2 -> DiceRoll (AddRolls t1 t2)
    RollMinus :: DiceRoll t1 -> DiceRoll t2 -> DiceRoll (SubRolls t1 t2)
    RollMul :: DiceRoll t1 -> DiceRoll t2 -> DiceRoll (MulRolls t1 t2)

deriving instance Show (DiceRoll t)

infixl 6 `RollPlus`
infixl 6 `RollMinus`
infixl 7 `RollMul`

diceNotation :: Int -> DiceRoll t -> ShowS
diceNotation _ (RollConstant k) = showString $ show k
diceNotation _ (RollCount n d) = showString $ show n ++ "d" ++ show d
diceNotation _ (RollKeepHighest k (RollCount n d)) =
    showString $ show n ++ "d" ++ show d ++ "kh" ++ show k
diceNotation _ (RollKeepLowest k (RollCount n d)) =
    showString $ show n ++ "d" ++ show d ++ "kl" ++ show k
diceNotation p (RollPlus x y) =
    showParen (p > 6) $ diceNotation 7 x . showString "+" . diceNotation 7 y
diceNotation p (RollMinus x y) =
    showParen (p > 6) $ diceNotation 7 x . showString "-" . diceNotation 7 y
diceNotation p (RollMul x y) =
    showParen (p > 7) $ diceNotation 8 x . showString "*" . diceNotation 8 y

showDiceNotation :: DiceRoll t -> String
showDiceNotation = flip (diceNotation 0) ""

diceResult :: MonadRandom m => DiceRoll t -> m (RollResult t)
diceResult r = case r of
    RollConstant x -> pure x
    RollCount n d -> replicateM n (getRandomR (1, d))
    RollKeepHighest k (RollCount n d) -> do
        rolled <- replicateM n (getRandomR (1, d))
        pure $ splitAt k (sortOn Down rolled)
    RollKeepLowest k (RollCount n d) -> do
        rolled <- replicateM n (getRandomR (1, d))
        pure $ splitAt k (sort rolled)
    RollPlus x y -> (,) <$> diceResult x <*> diceResult y
    RollMinus x y -> (,) <$> diceResult x <*> diceResult y
    RollMul x y -> (,) <$> diceResult x <*> diceResult y

resultDisplayAndTotal :: forall m t. (MonadRandom m, SingI t) =>
                        DiceRoll t -> m (String, RollResult t, String, Int)
resultDisplayAndTotal d = do
    let notation = showDiceNotation d
    rolls <- diceResult d
    let total = withSing @t totalResult rolls
    let display = withSing @t showResult rolls
    pure (notation, rolls, display, total)

totalResult' :: forall m t. (MonadRandom m, SingI t) => DiceRoll t -> m Int
totalResult' d = withSing @t totalResult <$> diceResult d

totalResult :: Sing t -> RollResult t -> Int
totalResult SConstant k = k
totalResult SCount ks = sum ks
totalResult SKeepHighest (ks, _) = sum ks
totalResult SKeepLowest (ks, _) = sum ks
totalResult (SAddRolls t1 t2) (x, y) = totalResult t1 x + totalResult t2 y
totalResult (SSubRolls t1 t2) (x, y) = totalResult t1 x - totalResult t2 y
totalResult (SMulRolls t1 t2) (x, y) = totalResult t1 x * totalResult t2 y

showResult :: Sing t -> RollResult t -> String
showResult SConstant k = show k
showResult SCount ks = show ks
showResult SKeepHighest (ks, ds) = "(" ++ show ks ++ "(dropped " ++ show ds ++ "))"
showResult SKeepLowest (ks, ds) = "(" ++ show ks ++ "(dropped " ++ show ds ++ "))"
showResult (SAddRolls t1 t2) (x, y) = "(" ++ showResult t1 x ++ ") + (" ++ showResult t2 y ++ ")"
showResult (SSubRolls t1 t2) (x, y) = "(" ++ showResult t1 x ++ ") - (" ++ showResult t2 y ++ ")"
showResult (SMulRolls t1 t2) (x, y) = "(" ++ showResult t1 x ++ ") * (" ++ showResult t2 y ++ ")"
