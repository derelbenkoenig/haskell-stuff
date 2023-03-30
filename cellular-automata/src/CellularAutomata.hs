{-# LANGUAGE TypeFamilies #-}
module CellularAutomata where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Bool
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Distributive
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

-- using constant length,
-- maybe at some point make it a type parameter of kind Nat
cellArrayLen :: Int
cellArrayLen = 128

newtype CellArray a = CellArray { getCellArray :: V.Vector a }
    deriving Show

displayAutomaton :: Automaton -> T.Text
displayAutomaton (Automaton w) =
    vecToText $ fmap (bool ' ' '#') $ getCellArray $ getStore w

vecToText :: V.Vector Char -> T.Text
vecToText = T.pack . V.foldr' (:) ""

boolVecToInt :: V.Vector Bool -> Int
boolVecToInt = V.foldl' (\n digit -> n * 2 + fromEnum digit) 0

rule30 :: ((V.Vector Bool -> Bool) -> (Int -> V.Vector Int) -> a) -> a
rule30 k = k update getNeighbors where
    update v = let n = boolVecToInt v in n >=1 && n <= 4
    getNeighbors i = V.enumFromTo (i-1) (i+1)

printSteps :: (V.Vector Bool -> Bool)
           -> (Int -> V.Vector Int)
           -> Int
           -> Automaton
           -> IO ()
printSteps update getNeighbors n a
  | n <= 0 = return ()
  | otherwise = T.putStrLn (displayAutomaton a) >>
      printSteps update getNeighbors (n-1)
      (stepAutomaton update getNeighbors a)

sliceWrapping :: Int -> Int -> V.Vector a -> V.Vector a
sliceWrapping start len v =
    let endIdx = start + len
        capacity = V.length v
     in if endIdx <= capacity
           then V.slice start len v
           else let endLen = endIdx - capacity
                    startLen = endLen - len
                 in V.slice start endLen v V.++ V.slice 0 startLen v

middleCellOn :: Automaton
middleCellOn = let middleIndex = cellArrayLen `div` 2
                in Automaton $ store (== middleIndex) middleIndex

instance Functor CellArray where
    fmap f (CellArray v) = CellArray $ fmap f v

instance Distributive CellArray where
    distribute f =
        CellArray $ V.generate cellArrayLen (\i -> fmap (`index` i) f)

instance Representable CellArray where
    type Rep CellArray = Int
    index (CellArray v) i = v V.! (i `mod` V.length v)
    tabulate f = CellArray $ V.generate cellArrayLen f

getStore :: Store g a -> g a
getStore (StoreT (Identity ga) _) = ga

newtype Automaton = Automaton (Store CellArray Bool)

stepAutomaton :: (V.Vector Bool -> Bool) -- ^ get new value from neighborhood
              -> (Int -> V.Vector Int) -- ^ get neighborhood indices
              -> Automaton
              -> Automaton
stepAutomaton update getNeighbors (Automaton a) = Automaton $
    extend (update . experiment getNeighbors) a
