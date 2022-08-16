module Main where

import Wc
import Options.Applicative
import qualified Data.Set as Set
import Data.Maybe

main :: IO ()
main = execParser (info (helper <*> parseOpts) myInfoMod) >>= print

myInfoMod = fullDesc
    <> header ("Print  newline,  word, and byte counts for each FILE, and a total line "
        ++ "if more than one FILE is specified.  A word is a non-zero-length sequence of "
        ++ "printable  characters  delimited  by  white space.")

data Opts = Opts { mode :: (Set.Set CountMode), files :: [FileArgument] }
    deriving Show

parseOpts :: Parser Opts
parseOpts = collectOpts
    <$> linesFlag
    <*> wordsFlag
    <*> charsFlag
    <*> bytesFlag
    <*> maxLineLengthFlag
    <*> fileArg

mkModeFlag :: CountMode -> Char -> String -> String -> Parser (Maybe CountMode)
mkModeFlag mode s l h = flag Nothing (Just mode)
    (
    short s
    <> long l
    <> help ("print the " ++ h) )

linesFlag = mkModeFlag Lines 'l' "lines" "line counts"
wordsFlag = mkModeFlag Words 'w' "words" "word counts"
charsFlag = mkModeFlag Chars 'm' "chars" "character counts"
bytesFlag = mkModeFlag Bytes 'c' "bytes" "byte counts"
maxLineLengthFlag = mkModeFlag MaxLineLength 'L' "max-line-length" "maximum display width"

data FileArgument = Stdin | Filename FilePath
    deriving Show

readFileArg = fmap f str where
    f s = if s == "-" then Stdin else Filename s

fileArg = some (argument readFileArg (metavar "[FILE]...")) <|> pure [Stdin]

defaultMode s = if Set.null s then Set.fromList [Lines, Words, Bytes] else s

collectOpts :: Maybe CountMode
            -> Maybe CountMode
            -> Maybe CountMode
            -> Maybe CountMode
            -> Maybe CountMode
            -> [FileArgument]
            -> Opts
collectOpts l w c b m fs = Opts (defaultMode $ Set.fromList $ catMaybes [l,w,c,b,m]) fs
