module Json where

import Data.Map (Map)
import qualified Data.Map as Map
import StringParser
import Control.Applicative
import Data.Char
import Data.Foldable (foldl')
import Data.Ratio

data JsonValue =
    JsonObject (Map String JsonValue)
    | JsonArray [JsonValue]
    | JsonString String
    | JsonNull
    | JsonBool Bool
    | JsonNumber Double
    deriving (Show, Eq)

escapedChar :: Parser Char
escapedChar = singleChar '\\' *> (singleChar '"' <|> singleChar '\\')

parseJsonString :: Parser JsonValue
parseJsonString = JsonString <$> parseStringLiteral

parseStringLiteral :: Parser String
parseStringLiteral =
    between (singleChar '"') (singleChar '"') (many (escapedChar <|> satisfy (/= '"')))

parseJson :: Parser JsonValue
parseJson =
    parseJsonNull
    <|> parseJsonString
    <|> parseJsonBool
    <|> parseJsonNumber
    <|> parseJsonArray
    <|> parseJsonObject

parseJsonNull :: Parser JsonValue
parseJsonNull = JsonNull <$ matchStr "null"

parseJsonBool :: Parser JsonValue
parseJsonBool = JsonBool <$>
    (True <$ matchStr "true" <|> False <$ matchStr "false")

parseDigits :: Parser String
parseDigits = many (try $ satisfy isDigit)

parseDot :: Parser Char
parseDot = singleChar '.'

parseSignum :: Parser Int
parseSignum = 1 <$ singleChar '+' <|> (-1) <$ singleChar '-'

parseE :: Parser Char
parseE = singleChar 'e' <|> singleChar 'E'

numFromDigits :: [Char] -> Int
numFromDigits = foldl'
    (\num dig -> num * 10 + digitToInt dig)
    0

fracFromDigits :: [Char] -> Ratio Integer
fracFromDigits = foldl'
    (\num dig -> (10 * numerator num + fromIntegral (digitToInt dig)) % (10 * denominator num))
    0

parseJsonNumber :: Parser JsonValue
parseJsonNumber = do
    firstSign <- parseSignum <|> pure 1
    firstDigits <- some $ try $ satisfy isDigit
    let wholePart = numFromDigits firstDigits
    dot <- optional parseDot
    fracPart <- case dot of
        Just _ -> fracFromDigits <$> parseDigits
        Nothing -> pure 0
    exponentPart <- (do
        _ <- parseE
        exponentSignum <- parseSignum <|> pure 1
        exponentDigits <- parseDigits
        pure $ exponentSignum * numFromDigits exponentDigits) <|> pure 0
    pure $ JsonNumber $ fromRational $
        fromIntegral firstSign * (fromIntegral wholePart + fracPart) * (10 ^^ exponentPart)

parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$>
    between
        (token $ singleChar '[') (token $ singleChar ']')
        (token parseJson `sepBy` token (singleChar ','))

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject . Map.fromList <$>
    between
        (token (singleChar '{'))
        (token (singleChar '}'))
        (
            ((,) <$> (token parseStringLiteral <* token (singleChar ':')) <*> token parseJson)
            `sepBy` token (singleChar ',')
        )
