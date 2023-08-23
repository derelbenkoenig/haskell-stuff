module MyRand where

import System.Random
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import Control.Monad.State

randomElem :: RandomGen g => State g Float
randomElem = do
  g <- get
  let (el, g') = random g
  put g'
  return el

randVec :: RandomGen g => g -> Int -> (Vector Float, g)
randVec g n = runState (V.replicateM n randomElem) g
