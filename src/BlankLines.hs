module Main where

import System.Environment
import Data.List (foldl')
import Control.Monad (replicateM_)

main :: IO ()
main = do
    args <- getArgs
    let numLines = foldl' (+) 0 (fmap read args) :: Int
    blanklines numLines

blanklines :: Int -> IO ()
blanklines n = replicateM_ n (putStrLn "")
