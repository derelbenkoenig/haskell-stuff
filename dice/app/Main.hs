module Main (main) where

import Dice
import Dice.Parsing
import Data.Bifunctor
import Exinst (Some1, withSome1)
import Data.Text (pack)

import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
    eDice <- readDiceRoll
    case eDice of
        Left e -> putStrLn e
        Right someDice -> withSome1 someDice $ \ dice -> do
            (notation, _, display, total) <- resultDisplayAndTotal dice
            putStr notation
            putStr ": "
            putStr display
            putStr " = "
            putStr (show total)
            putStrLn ""
    main

readDiceRoll :: IO (Either String (Some1 DiceRoll))
readDiceRoll = first errorBundlePretty . parse diceRoll "" . pack <$> getLine
