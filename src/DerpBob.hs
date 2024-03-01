module Main where

import ArgParser
import Control.Monad.Random
import Data.Char
import Data.Monoid (Endo(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (isEOF)

main :: IO ()
main = do
    mode <- parseProgramArgs parseArgs
    accum <- accumForMode mode
    linewise accum

data DerpBobMode = Alternating | Randomized !Double
    deriving (Eq, Show)

toAlternating :: Endo DerpBobMode
toAlternating = Endo $ const Alternating

toDefaultRandom :: Endo DerpBobMode
toDefaultRandom = Endo $ \mode -> case mode of
    Randomized p -> Randomized p
    Alternating -> defaultRandom

withProbability :: Double -> Endo DerpBobMode
withProbability p = Endo $ \mode -> case mode of
    Randomized _ -> Randomized p
    Alternating -> Randomized p

parseMode :: ArgParser (Endo DerpBobMode)
parseMode =
    toAlternating <$ shortLongFlag 'a' "alternating"
    <|> toDefaultRandom <$ shortLongFlag 'r' "random"
    <|> withProbability <$> (shortLongOpt 'p' "probability" autoReader)

parseArgs :: ArgParser DerpBobMode
parseArgs = (`appEndo` Alternating) <$> go where
    go = mempty <$ endOfArgs <|> flip (<>) <$> parseMode <*> go

defaultRandom :: DerpBobMode
defaultRandom = Randomized 0.9

accumForMode :: DerpBobMode -> IO (Text -> Text)
accumForMode Alternating = pure $ derpbob altCase False
accumForMode (Randomized d) = do
    g <- getStdGen
    pure $ derpbob (randomAccum (randomFlip d)) (RandState g Lower)

type TextAccumFunc s = s -> Char -> (s, Char)
type RandGenFunc g a = g -> (a, g)

derpbob :: TextAccumFunc s -> s -> Text -> Text
derpbob f z = snd . Text.mapAccumL f z

altCase :: TextAccumFunc Bool
altCase b c = (not b, changeIf b swapCase c)

randomFlip :: RandomGen g => Double -> g -> (Bool, g)
randomFlip prob gen = let
    ~(randVal, newGen) = random gen
    in (randVal < prob, newGen)

data CharCase = Upper | Lower

flipCase :: CharCase -> CharCase
flipCase Upper = Lower
flipCase Lower = Upper

caseChar :: CharCase -> Char -> Char
caseChar Upper = toUpper
caseChar Lower = toLower

data RandState g = RandState !g !CharCase

randomAccum :: RandGenFunc g Bool -> TextAccumFunc (RandState g)
randomAccum f (RandState gen casing) c = let
    ~(doFlip, newG) = f gen
    newCase = if doFlip then flipCase casing else casing
    newState = RandState newG newCase
    in (newState, caseChar casing c)

changeIf :: Bool -> (a -> a) -> a -> a
changeIf cond f x = if cond then f x else x

changeIfM :: Monad m => Bool -> (a -> m a) -> a -> m a
changeIfM cond f x = if cond then f x else pure x

swapCase :: Char -> Char
swapCase c
    | isLower c = toUpper c
    | isUpper c = toLower c
    | otherwise = c

linewise :: (Text -> Text) -> IO ()
linewise f = do
    line <- Text.getLine
    let lineOut = f line
    Text.putStrLn lineOut
    stop <- isEOF
    if stop then pure () else linewise f
