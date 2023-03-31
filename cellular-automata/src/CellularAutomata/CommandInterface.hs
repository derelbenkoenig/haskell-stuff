{-# LANGUAGE OverloadedStrings #-}

module CellularAutomata.CommandInterface where

import CellularAutomata

import Control.Exception hiding (try)
import Control.Monad.State.Strict

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Data.Word (Word8)

import System.IO (Handle)
import System.IO.Error

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data CommandState = CommandState Automaton Word8
type CommandM a = StateT CommandState IO a
type Parser = Parsec Void T.Text

execCommands :: Handle -> CommandM CommandState
execCommands h  = do
    joinIO $ handleJust (guard . isEOFError) (return . const get) $ do
        line <- T.hGetLine h
        return $ do
            let parseResult = runParser parseCommand "" line
            either (liftIO . putStrLn . errorBundlePretty) id parseResult
            execCommands h

joinIO :: MonadIO m => IO (m a) -> m a
joinIO = join . liftIO

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspace

parseCommand :: Parsec Void T.Text (CommandM ())
parseCommand = hspace *>
    (do
        steps <- symbol "run" *> lexeme L.decimal <* eof
        return $ do
            replicateM_ steps $ do
                CommandState automaton ruleNo <- get
                let newAutomaton = numRule ruleNo stepAutomaton automaton
                put $ CommandState newAutomaton ruleNo
                liftIO $ T.putStrLn $ displayAutomaton newAutomaton
    <|> do
        ruleNo <- symbol "rule" *> word8 <* eof
        return $ modify $ \(CommandState a _) -> CommandState a ruleNo
    <|> do
        _ <- symbol "randomize" <* eof
        return $ do
            newAutomaton <- randomCellsOn
            modify $ \(CommandState _ n) -> CommandState newAutomaton n
            liftIO $ T.putStrLn $ displayAutomaton newAutomaton
    <|> do
        _ <- eof
        return $ return ()
    )

word8 :: Parser Word8
word8 = try (char '0' *> char 'b' *> L.binary) <|> L.decimal
