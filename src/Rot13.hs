module Main
    ( main
    ) where

import Data.ByteString.Char8 as BC
import Data.Char
import Data.Conduit
import Data.Conduit.Combinators as CC (stdin, stdout, map)

main :: IO ()
main = runConduit $ stdin .| (CC.map . BC.map) rot13 .| stdout

rot13 :: Char -> Char
rot13 c
  | isAlpha c =
      let base = ord $ if isUpper c then 'A' else 'a'
       in toEnum $ base + (fromEnum c - base + 13) `mod` 26
  | otherwise = c
