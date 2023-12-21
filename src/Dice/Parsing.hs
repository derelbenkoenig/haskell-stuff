{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Dice.Parsing where

-- import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Text (Text)
import Dice
import Exinst
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor.Compose (Compose)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- diceRoll :: Parser (Some1 DiceRoll)
-- diceRoll =
--     some1 <$> constant <|>
--     some1 <$> countDice <|>
--     some1FromBlackbird <$> plus

constant :: Parser (DiceRoll Constant)
constant = RollConstant <$> lexeme L.decimal

countDice :: Parser (DiceRoll Count)
countDice = do
    ct <- lexeme L.decimal <|> pure 1
    _ <- lexeme $ single 'd'
    dieSize <- lexeme L.decimal
    pure $ RollCount ct dieSize

binarySome1 :: (SRollType t1 -> SRollType t2 -> SRollType rt) ->
               (DiceRoll t1 -> DiceRoll t2 -> DiceRoll rt) ->
               Some1 DiceRoll -> Some1 DiceRoll -> Some1 DiceRoll
binarySome1 tyOp valOp (Some1 t1 d1) (Some1 t2 d2) = Some1 (tyOp t1 t2) (valOp d1 d2)

operatorTable :: [[Operator Parser (Some1 DiceRoll)]]
operatorTable =
    let op o sym = o <$ lexeme (single sym) in
    [ [ InfixN $ op (\ (Some1 t1 d1) (Some1 t2 d2) -> Some1 (SAddRolls t1 t2) (RollPlus d1 d2)) 'd' ]
      -- [ InfixL $ op (some1 `blackbird` RollMul) '*' ],
      -- [ InfixL $ op '+', InfixL $ op '-' ]
    ]

