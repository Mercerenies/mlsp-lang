module Lang.Operator(operatorExpr, operatorTable,
                     typeOperatorExpr, typeOperatorTable,
                     OpExpr(..), TypeOp(..), Op(..)) where

import Lang.Tokens
import Control.Monad.Identity(Identity)
import Text.Parsec.Expr

data OpExpr o a = OpExpr a
                | Pre o (OpExpr o a)
                | Post (OpExpr o a) o
                | Inf (OpExpr o a) o (OpExpr o a)
                  deriving (Show, Read, Eq)

data TypeOp = Union | Inter
              deriving (Show, Read, Eq, Ord, Enum)

data Op = Plus | Minus | Times | Div | BooleanAnd | BooleanOr | Apply |
          UniversalAnd | UniversalOr | Equal | NEqual | Less | Greater | LE | GE
          deriving (Show, Read, Eq, Ord, Enum)

prefix :: o -> EParser b -> Operator [Lexeme] () Identity (OpExpr o a)
prefix op lex = Prefix $ Pre op <$ lex

postfix :: o -> EParser b -> Operator [Lexeme] () Identity (OpExpr o a)
postfix op lex = Postfix $ flip Post op <$ lex

binary :: Assoc -> o -> EParser b -> Operator [Lexeme] () Identity (OpExpr o a)
binary assoc op lex = flip Infix assoc $ (\x y -> Inf x op y) <$ lex

-- TODO Add in (<|) and (|>) for function composition
operatorTable :: OperatorTable [Lexeme] () Identity (OpExpr Op a)
operatorTable = [
   [prefix Plus $ operator "+"],
   [prefix Minus $ operator "-"],
   [binary AssocLeft Times $ operator "*"],
   [binary AssocLeft Div $ operator "/"],
   [binary AssocLeft Plus $ operator "+", binary AssocLeft Minus $ operator "-"],
   [binary AssocNone Equal $ operator "==", binary AssocNone Less $ operator "<",
    binary AssocNone Greater $ operator ">", binary AssocNone LE $ operator "<=",
    binary AssocNone GE $ operator ">=", binary AssocNone NEqual $ operator "!="],
   [binary AssocLeft BooleanAnd $ operator "&&"],
   [binary AssocLeft BooleanOr $ operator "||"],
   [binary AssocLeft Apply $ operator "=~"],
   [binary AssocLeft UniversalAnd $ keyword "and"],
   [binary AssocLeft UniversalOr $ keyword "or"]
 ]

operatorExpr :: EParser a -> EParser (OpExpr Op a)
operatorExpr = buildExpressionParser operatorTable . (OpExpr <$>)

{-
 - TODO Consider other type operators as syntax sugar (since no other operators are
 -      used at the type level currently). For instance:
 - a * b == (a, b)
 -}

typeOperatorTable :: OperatorTable [Lexeme] () Identity (OpExpr TypeOp a)
typeOperatorTable = [
   [binary AssocLeft Inter $ operator "&"],
   [binary AssocLeft Union $ operator "|"]
 ]

typeOperatorExpr :: EParser a -> EParser (OpExpr TypeOp a)
typeOperatorExpr = buildExpressionParser typeOperatorTable . (OpExpr <$>)
