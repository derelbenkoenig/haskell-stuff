module Main where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (isEOF)

main :: IO ()
main = linewise derpbob

derpbob :: Text -> Text
derpbob = snd . Text.mapAccumL (\ b c -> (not b, swapCaseIf b c)) False

swapCaseIf :: Bool -> Char -> Char
swapCaseIf cond c = if cond then swapCase c else c

swapCase :: Char -> Char
swapCase c
    | isLower c = toUpper c
    | isUpper c = toLower c
    | otherwise = c

linewise :: (Text -> Text) -> IO ()
linewise f = do
    line <- Text.getLine
    Text.putStrLn (f line)
    stop <- isEOF
    if stop then pure () else linewise f
