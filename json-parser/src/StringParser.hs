{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use lambda-case" #-}

module StringParser where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)

newtype ErrMsg = ErrMsg String
    deriving Show

newtype Parser a = Parser { runParser :: String -> (String, Either ErrMsg a) }

instance Functor Parser where
    fmap f (Parser g) = Parser $ \s ->
        let (rest, result) = g s
        in case result of
            Left err -> (rest, Left err)
            Right x -> (rest, Right (f x))
    -- or
    -- fmap f = Parser . (fmap . fmap . fmap) f . runParser

instance Applicative Parser where
    pure a = Parser $ \s -> (s, Right a)
    Parser f <*> Parser x = Parser $ \s ->
        -- run the first parser on the input
        let (rest1, result1) = f s
        in case result1 of
            -- if the first parser failed, stop (but we already consumed part of the input)
            Left e1 -> (rest1, Left e1)
            -- if the first parser succeeded, run the next parser where the first one left off
            Right fun -> let (rest2, result2) = x rest1
                in case result2 of
                    -- if the second one failed, return its error
                    Left e2 -> (rest2, Left e2)
                    -- if the second one also succeeded, combine their results together
                    Right arg -> (rest2, Right (fun arg))

instance Alternative Parser where
    empty = Parser $ \s -> (s, Left $ ErrMsg "parse error")
    Parser x <|> Parser y = Parser $ \s ->
        let (rest1, xResult) = x s
        in case xResult of
            -- if the first one failed, run the second one and just return whatever it returns
            Left _ -> y rest1
            -- if the first one succeded, ignore the second one
            Right xValue -> (rest1, Right xValue)
            -- it's like a short circuiting 'or', or like || in shell commands

instance Monad Parser where
    Parser x >>= f = Parser $ \s ->
        let (rest1, xResult) = x s
        in case xResult of
            Left e -> (rest1, Left e)
            -- f uses the *value* of the first result to decide how it even *wants* to parse
            Right xValue -> let (rest2, fxResult) = runParser (f xValue) rest1
                in case fxResult of
                    Left e -> (rest2, Left e)
                    Right fxValue -> (rest2, Right fxValue)

-- this is just like if something is Alternative AND a Monad
instance MonadPlus Parser where
    -- mzero = empty
    -- mplus = (<|>)

-- this lets you conveniently fail with an error message
instance MonadFail Parser where
    fail msg = Parser $ \s -> (s, Left (ErrMsg msg))

-- Run a parser, but if it fails, "rewind" instead of consuming input
try :: Parser a -> Parser a
try (Parser f) = Parser $ \s ->
    let (rest, result) = f s
    in case result of
        Left e -> (s, Left e)
        Right a -> (rest, Right a)

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    "" -> ("", Left (ErrMsg "unexpected end of input"))
    (c:cs) -> (cs, Right c)

-- satisfy is a special case in that it doesn't consume its input if it fails
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s ->
    case s of
        [] -> ("", Left (ErrMsg "unexpected end of input"))
        c:cs | predicate c -> (cs, Right c)
        c:cs -> (c:cs, Left (ErrMsg ("unexpected " ++ [c])))

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP predicate = Parser $ \s ->
    let (prefix, suffix) = span predicate s in (suffix, Right prefix)

singleChar :: Char -> Parser Char
singleChar expected = satisfy (== expected)

matchStr :: String -> Parser String
matchStr target = case target of
    "" -> pure ""
    (c:cs) -> (:) <$> singleChar c <*> matchStr cs

between :: Parser open -> Parser close -> Parser thing -> Parser thing
between open close thing = open *> thing <* close

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> ("", Right ())
    cs -> (cs, Left (ErrMsg "expected end of file"))

-- parses ZERO OR MORE
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy a sep = sepBy1 a sep <|> pure []

-- parses ONE OR MORE
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 a sep = (:) <$> a <*> many (sep *> a)

-- parses a thing, and then consumes any amount of whitespace after
token :: Parser a -> Parser a
token a = a <* takeWhileP isSpace
