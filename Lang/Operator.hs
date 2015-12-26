module Lang.Operator(operatorExpr, operatorTable, OpExpr(..), Op(..)) where

import Lang.Tokens
import Control.Monad.Identity(Identity)
import Text.Parsec.Expr

data OpExpr a = OpExpr a
              | Pre Op (OpExpr a)
              | Post (OpExpr a) Op
              | Inf (OpExpr a) Op (OpExpr a)
                deriving (Show, Read, Eq)

data Op = Plus | Minus | Times | Div | BooleanAnd | BooleanOr | Apply |
          UniversalAnd | UniversalOr | Equal | NEqual | Less | Greater | LE | GE
          deriving (Show, Read, Eq, Ord, Enum)

prefix :: Op -> EParser b -> Operator [Lexeme] () Identity (OpExpr a)
prefix op lex = Prefix $ Pre op <$ lex

postfix :: Op -> EParser b -> Operator [Lexeme] () Identity (OpExpr a)
postfix op lex = Postfix $ flip Post op <$ lex

binary :: Assoc -> Op -> EParser b -> Operator [Lexeme] () Identity (OpExpr a)
binary assoc op lex = flip Infix assoc $ (\x y -> Inf x op y) <$ lex

operatorTable :: OperatorTable [Lexeme] () Identity (OpExpr a)
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
   [binary AssocLeft UniversalAnd $ operator "and"],
   [binary AssocLeft UniversalOr $ operator "or"]
 ]

operatorExpr :: EParser a -> EParser (OpExpr a)
operatorExpr = buildExpressionParser operatorTable . (OpExpr <$>)
