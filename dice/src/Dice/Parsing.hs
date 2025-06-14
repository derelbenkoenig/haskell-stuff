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
import Data.Bifunctor

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

flap :: Functor f => f (a -> b) -> a -> f b
flap f x = fmap ($ x) f

atomicRoll :: Parser (Some1 DiceRoll)
atomicRoll = do
    firstNum <- lexeme L.decimal
    (do
        rollCount <- flap dieSizeClause firstNum
        either some1 some1 <$> flap keepClause rollCount <|>
            pure (some1 rollCount)
        ) <|>
        pure (some1 $ RollConstant firstNum)

dieSizeClause :: Parser (Int -> DiceRoll Count)
dieSizeClause = lexeme (single 'd') *> (flip RollCount <$> lexeme L.decimal)

keepClause :: Parser (DiceRoll Count -> Either (DiceRoll KeepLowest) (DiceRoll KeepHighest))
keepClause = do
    keepWhich <- lexeme $ single 'k' *>
            (Left SKeepLowest <$ single 'l' <|> Right SKeepHighest <$ single 'h')
    keepHowMany <- lexeme L.decimal
    pure $ \roll -> bimap
        -- the keepClauseHelper is extra type safety. If I accidentally put the wrong constructor
        -- in either of these expressions it won't type check
        (\rollType -> keepClauseHelper rollType RollKeepLowest keepHowMany roll)
        (\rollType -> keepClauseHelper rollType RollKeepHighest keepHowMany roll)
        keepWhich

keepClauseHelper :: SRollType t ->
                    (Int -> DiceRoll Count -> DiceRoll t) ->
                    Int -> DiceRoll Count -> DiceRoll t
keepClauseHelper _ = id

countDice :: Parser (DiceRoll Count)
countDice = do
    ct <- lexeme L.decimal <|> pure 1
    _ <- lexeme $ single 'd'
    dieSize <- lexeme L.decimal
    pure $ RollCount ct dieSize

binarySome1 :: forall tyOp.
               (forall t1 t2. SRollType t1 -> SRollType t2 -> SRollType (tyOp t1 t2)) ->
               (forall t1 t2. DiceRoll t1 -> DiceRoll t2 -> DiceRoll (tyOp t1 t2)) ->
               Some1 DiceRoll -> Some1 DiceRoll -> Some1 DiceRoll
binarySome1 tyOp valOp (Some1 t1 d1) (Some1 t2 d2) = Some1 (tyOp t1 t2) (valOp d1 d2)

op ::
    (Some1 DiceRoll -> Some1 DiceRoll -> Some1 DiceRoll) ->
    Char ->
    Parser (Some1 DiceRoll -> Some1 DiceRoll -> Some1 DiceRoll)
op o sym = o <$ lexeme (single sym)

operatorTable :: [[Operator Parser (Some1 DiceRoll)]]
operatorTable =
    [ [ InfixL $ op (binarySome1 SMulRolls RollMul) '*' ],
      [ InfixL $ op (binarySome1 SAddRolls RollPlus) '+',
        InfixL $ op (binarySome1 SSubRolls RollMinus) '-' ]
    ]

parens :: Parser a -> Parser a
parens = between (lexeme (single '(')) (lexeme (single ')'))

diceRoll :: Parser (Some1 DiceRoll)
diceRoll = makeExprParser (parens diceRoll <|> atomicRoll) operatorTable
