{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module CellularAutomata where

import Control.Comonad
import Control.Comonad.Representable.Store
import Control.Monad.Random
import Data.Bool
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Distributive
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Word (Word8)

-- using constant length,
-- maybe at some point make it a type parameter of kind Nat
cellArrayLen :: Int
cellArrayLen = 128

newtype CellArray a = CellArray { getCellArray :: V.Vector a }
    deriving Show

displayAutomaton :: Automaton -> T.Text
displayAutomaton (Automaton w) =
    vecToText $ fmap (bool '░' '█') $ getCellArray $ getStore w

vecToText :: V.Vector Char -> T.Text
vecToText = T.pack . V.foldr' (:) ""

boolVecToInt :: V.Vector Bool -> Int
boolVecToInt = V.foldl' (\n digit -> n * 2 + fromEnum digit) 0

integralToBoolVec :: Integral a => a -> V.Vector Bool
integralToBoolVec 0 = V.empty
integralToBoolVec i
  | i < 0 = error "cannot convert negative number to bits"
  | otherwise =
      let (halfI, bit) = i `divMod` 2
       in integralToBoolVec halfI V.++ V.singleton (toEnum $ fromIntegral bit)

fillLeft :: a -> Int -> V.Vector a -> V.Vector a
fillLeft z n v =
    let offset = max 0 (n - V.length v)
     in V.generate n (\i -> if i < offset then z else v V.! (i - offset))

-- while in theory the update of a cell could depend on neighbors further
-- away than the immediate ones, most of the common rules do only use the 
-- immediate ones
getImmediateNeighbors :: Int -> V.Vector Int
getImmediateNeighbors i = V.enumFromTo (i - 1) (i + 1)

numRule :: Word8 -> ((V.Vector Bool -> Bool) -> (Int -> V.Vector Int) -> a) -> a
numRule n k =
    k 
    -- the rule itself is represented as 8 bits, a zero or one for each
    -- possible value of the neighbors, which itself is viewed as a binary
    -- number by treating its values as bits
    (\neighbors ->
        fillLeft False 8 (integralToBoolVec n) V.! (7 - boolVecToInt neighbors))
    getImmediateNeighbors

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

-- using undefined for the starting index since it should never
-- matter, we only do operations that operate on all indices via 'extend'
middleCellOn :: Automaton
middleCellOn = let middleIndex = cellArrayLen `div` 2
                in Automaton $ store (== middleIndex) undefined

lastCellOn :: Automaton
lastCellOn = let lastIndex = cellArrayLen - 1
              in Automaton $ store (== lastIndex) undefined

randomCellsOn :: MonadRandom m => m Automaton
randomCellsOn =
    (\v -> Automaton $ StoreT (Identity $ CellArray v) undefined)
    <$> V.replicateM cellArrayLen getRandom

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

newtype Automaton = Automaton { getAutomaton :: Store CellArray Bool }

stepAutomaton :: (V.Vector Bool -> Bool) -- ^ get new value from neighborhood
              -> (Int -> V.Vector Int) -- ^ get neighborhood indices
              -> Automaton
              -> Automaton
stepAutomaton update getNeighbors (Automaton a) = Automaton $
    extend (update . experiment getNeighbors) a
