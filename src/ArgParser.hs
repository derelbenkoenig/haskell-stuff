module ArgParser (
    ArgParser(..),
    parseEither,
    endOfArgs,
    shortOpt,
    longOpt,
    shortLongOpt,
    argument,
    exact,
    errMsg,
    parseProgramArgs,
    shortFlag,
    longFlag,
    shortLongFlag,
    foldlArgsUntil,
    foldArgsUntil,
    foldMapArgsUntil,

    ArgReader(..),
    autoReader,
    module Control.Monad.Combinators
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators
import Text.Read (readEither)
import System.Environment (getArgs)

newtype ArgParser a = ArgParser { runArgParser :: [String] -> ([String], Either String a) }

-- just triple fmap since s -> (s, Either e a) is a composition of functors
-- (->) s ∘ (,) s ∘ Either e
instance Functor ArgParser where
    fmap f = ArgParser . (fmap . fmap . fmap) f . runArgParser

instance Applicative ArgParser where
    pure a = ArgParser $ \args -> (args, Right a)
    ArgParser f <*> ArgParser a = ArgParser $ \args -> let
        (args', ef) = f args
        in case ef of
            Left e -> (args', Left e)
            Right f' -> let
                (args'', a') = a args'
                in (args'', fmap f' a')

instance Alternative ArgParser where
    empty = ArgParser $ \args -> (args, Left "No parse")
    ArgParser a <|> ArgParser b = ArgParser $ \args -> let
        (args', ea) = a args
        in case ea of
            Left _ -> b args'
            Right a' -> (args', Right a')

instance Monad ArgParser where
    ArgParser a >>= f = ArgParser $ \args -> let
        (args', ea) = a args
        in case ea of
            Left e -> (args', Left e)
            Right a' -> runArgParser (f a') args'

instance MonadPlus ArgParser where
    mzero = empty
    mplus = (<|>)

instance MonadFail ArgParser where
    fail e = ArgParser $ \args -> (args, Left e)

newtype ArgReader a = ArgReader { readArg :: String -> Either String a }

instance Functor ArgReader where
    fmap f (ArgReader r) = ArgReader $ fmap f . r

instance Applicative ArgReader where
    pure a = ArgReader $ const (Right a)
    ArgReader f <*> ArgReader a = ArgReader $ \s -> f s <*> a s

newtype EitherString a = EitherString { getEitherString :: Either String a }

instance Functor EitherString where
    fmap f (EitherString e) = EitherString (fmap f e)

instance Applicative EitherString where
    pure = EitherString . Right
    EitherString f <*> EitherString a = EitherString (f <*> a)

instance Alternative EitherString where
    empty = EitherString (Left "No parse")
    EitherString a <|> EitherString b = EitherString $ case a of
        Left _ -> b
        Right _ -> a

instance Alternative ArgReader where
    empty = ArgReader $ const (Left "No parse")
    ArgReader a <|> ArgReader b = ArgReader $ \s ->
        getEitherString $ (EitherString $ a s) <|> (EitherString $ b s)

instance Monad ArgReader where
    ArgReader a >>= f = ArgReader $ \s -> case a s of
        Left e -> Left e
        Right a' -> readArg (f a') s

autoReader :: Read a => ArgReader a
autoReader = ArgReader readEither

parseProgramArgs :: ArgParser a -> IO a
parseProgramArgs p = do
    args <- getArgs
    case parseEither p args of
        Left e -> fail e
        Right a -> pure a

parseEither :: ArgParser a -> [String] -> Either String a
parseEither = (snd .) . runArgParser

endOfArgs :: ArgParser ()
endOfArgs = ArgParser $ \args ->
    if null args then (args, Right ()) else (args, Left "Extra arguments")

shortFlag :: Char -> ArgParser ()
shortFlag c = ArgParser $ \args -> case args of
    ('-':c':[]):restArgs | c' == c -> (restArgs, Right ())
    x -> (x, Left "No parse")

longFlag :: String -> ArgParser ()
longFlag s = ArgParser $ \args -> case args of
    ('-':'-':opt):restArgs | opt == s -> (restArgs, Right ())
    x -> (x, Left "No parse")

shortLongFlag :: Char -> String -> ArgParser ()
shortLongFlag c s = try (shortFlag c) <|> longFlag s

shortOpt :: Char -> ArgReader a -> ArgParser a
shortOpt c r = shortFlag c *> argument r

longOpt :: String -> ArgReader a -> ArgParser a
longOpt s r = longFlag s *> argument r

shortLongOpt :: Char -> String -> ArgReader a -> ArgParser a
shortLongOpt c s r = shortOpt c r <|> longOpt s r

exact :: String -> ArgParser String
exact s = ArgParser $ \args -> case args of
    (arg1:restArgs) | arg1 == s -> (restArgs, Right arg1)
    x -> (x, Left "No parse")

argument :: ArgReader a -> ArgParser a
argument r = ArgParser $ \args -> case args of
    (arg1:restArgs) -> (restArgs, readArg r arg1)
    x -> (x, Left "No parse")

errMsg :: String -> ArgParser a -> ArgParser a
errMsg e p = ArgParser $ \args -> let
    (args', res) = runArgParser p args
    in case res of
        Left _ -> (args', Left e)
        Right a -> (args', Right a)

try :: ArgParser a -> ArgParser a
try p = ArgParser $ \args -> let
    (args', res) = runArgParser p args
    in case res of
        Left e -> (args, Left e)
        Right a -> (args', Right a)

foldlArgsUntil :: Alternative f =>
    (b -> a -> b) -- folding function
    -> f b -- parser that decides when to stop AND returns the zero value of the fold
    -> f a -- parser to get the next value to fold in
    -> f b -- parsed folded result
foldlArgsUntil combine stop next =
    stop <|> flip combine <$> next <*> foldlArgsUntil combine stop next

foldArgsUntil :: (Alternative f, Monoid a) =>
    f () -- detect end of args
    -> f a -- parse one arg
    -> f a -- folded up result
foldArgsUntil stop next = 
    mempty <$ stop <|> (<>) <$> next <*> foldArgsUntil stop next

foldMapArgsUntil :: (Alternative f, Monoid m) =>
    (a -> m) -- map to monoid
    -> f () -- detect end of args
    -> f a -- parse one arg
    -> f m -- folded result
foldMapArgsUntil g stop next =
    mempty <$ stop <|> ((<>) . g) <$> next <*> foldMapArgsUntil g stop next
