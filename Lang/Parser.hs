module Lang.Parser(Decl(..), Type(..), Expr(..),
                   Fields(..), Access(..), IfOp(..), Call(..),
                   parseCode, file, toplevel) where

import Lang.Tokens
import Lang.Operator
import Data.List(intercalate)
import Data.Either(partitionEithers)
import qualified Data.Map as Map
import Data.Map(Map)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Error(ParseError)
import Control.Applicative hiding ((<|>), many)
import Control.Monad

data Decl = Module String [Decl] |
            Function (Maybe Type) String [String] Expr |
            Type String Type [(String, Type)] Fields | -- Name, parent, variables, fields
            Concept String [String] [(String, Type)] | -- Name, args, variables
            Instance String [Type] Type [Decl] |
            Variable (Maybe Type) String Expr
            deriving (Show, Read, Eq)

data Type = Tuple [Type] |
            Named String [Type] |
            Func [Type] Type
            deriving (Show, Read, Eq)
{-
data Stmt = Stmt Expr (Maybe (IfOp, Expr))
            deriving (Show, Read, Eq)
-}
data Expr = FunctionCall Expr [Expr] |
            DotCall Expr String [Expr] |
            Block [Expr] |
            Literal Token |
            TupleExpr [Expr] |
            ListExpr [Expr] |
            Declare Decl |
            VarAsn String Expr |
            Subscript Expr [Expr] |
            Ident String |
--            Statement Stmt |
            Oper (OpExpr Expr) |
            IfStmt IfOp Expr Expr (Maybe Expr)
            deriving (Show, Read, Eq)

newtype Fields = Fields (Map String Access)
    deriving (Show, Read, Eq)

data Access = Read | ReadWrite
              deriving (Show, Read, Eq, Ord)

data IfOp = If | Unless
            deriving (Show, Read, Eq, Ord)

data Call = Paren | Bracket | Dot String
            deriving (Show, Read, Eq, Ord)

instance Monoid Fields where
    mempty = Fields Map.empty
    (Fields m1) `mappend` (Fields m2) = Fields $ m1 `mappend` m2

parseCode :: String -> [Lexeme] -> Either ParseError [Decl]
parseCode = parse file

file :: EParser [Decl]
file = newlines *> endBy toplevel newlines1 <* eof

toplevel :: EParser Decl
toplevel = moduleDecl <|> functionDecl <|> typeDecl <|>
           conceptDecl <|> instanceDecl

moduleDecl :: EParser Decl
moduleDecl = do
  keyword "module"
  newlines
  name <- sepBy identifier (matchToken $ Operator ".")
  newlines1
  contents <- many (toplevel <* newlines1)
  keyword "end"
  return $ Module (intercalate "." name) contents

functionDecl :: EParser Decl
functionDecl = do
  type_ <- option Nothing $ try (Just <$> typeSpec <* newlines1)
  name <- identifier
  type' <- case type_ of
             Nothing -> return Nothing
             Just (str, expr)
                 | str == name -> return $ Just expr
                 | otherwise -> fail "preceding type declaration should match function"
  operator "("
  newlines
  args <- sepBy identifier nlComma
  newlines
  operator ")"
  operator "="
  newlines
  stmt <- statement
  return $ Function type' name args stmt

typeDecl :: EParser Decl
typeDecl = do
  keyword "type"
  newlines
  name <- identifier
  parent <- option (Named "T" []) $ operator "(" *> typeExpr <* operator ")"
  newlines1
  (types, fields) <- typeInterior
  keyword "end"
  return $ Type name parent types fields

typeInterior :: EParser ([(String, Type)], Fields)
typeInterior = do
  (types, fields) <- partitionEithers <$> endBy typeStatement newlines1
  return (types, mconcat fields)

typeStatement :: EParser (Either (String, Type) Fields)
typeStatement = Right <$> fields_ <|> Left <$> type_
    where type_ = typeSpec
          fields_ = do
            keyword "fields"
            newlines1
            exprs <- fieldsExpr
            keyword "end"
            return exprs

fieldsExpr :: EParser Fields
fieldsExpr = (Fields . Map.fromList) <$> endBy identifier' newlines1
    where isIdentifier x = case x of { Left (Identifier _) -> True ; _ -> False }
          isWIdentifier x = case x of { Left (WIdentifier _) -> True ; _ -> False }
          identifier' = do
            result <- satisfy (liftA2 (||) isIdentifier isWIdentifier)
            case result of
              Left (Identifier x) -> return (x, Read)
              Left (WIdentifier x) -> return (x, ReadWrite)
              _ -> unexpected "access modifier"

typeSpec :: EParser (String, Type)
typeSpec = do
  name <- identifier
  operator "::"
  newlines
  tpe <- typeExpr
  return (name, tpe)

typeExpr :: EParser Type
typeExpr = tupleTypeExpr <|> try funcTypeExpr <|> namedTypeExpr <?> "type expression"

tupleTypeExpr :: EParser Type
tupleTypeExpr = do
  operator "{"
  newlines
  contents <- sepBy typeExpr nlComma
  newlines
  operator "}"
  return $ Tuple contents

namedTypeExpr :: EParser Type
namedTypeExpr = do
  name <- identifier
  args <- option [] $ do
                  operator "["
                  newlines
                  args' <- sepBy typeExpr nlComma
                  newlines
                  operator "]"
                  return args'
  return $ Named name args

funcTypeExpr :: EParser Type
funcTypeExpr = do
  operator "("
  newlines
  lhs <- sepBy typeExpr nlComma
  newlines
  operator ")"
  operator "->"
  newlines
  rhs <- typeExpr
  return $ Func lhs rhs

conceptDecl :: EParser Decl
conceptDecl = do
  keyword "concept"
  newlines
  name <- identifier
  args <- option [] $ do
                operator "["
                newlines
                args' <- sepBy identifier nlComma
                newlines
                operator "]"
                return args'
  newlines1
  internals <- endBy typeSpec newlines1
  keyword "end"
  return $ Concept name args internals

instanceDecl :: EParser Decl
instanceDecl = do
  keyword "instance"
  newlines
  name <- identifier
  args <- option [] $ do
                operator "["
                newlines
                args' <- sepBy typeExpr nlComma
                newlines
                operator "]"
                return args'
  impl <- typeExpr
  newlines1
  internals <- endBy functionDecl newlines1
  keyword "end"
  return $ Instance name args impl internals

statement :: EParser Expr
statement = do
  expr <- toplevelExpr
  op <- optionMaybe $ do
              op' <- If <$ keyword "if" <|> Unless <$ keyword "unless"
              cond <- basicExpr
              return (op', cond)
  case op of
    Nothing -> return expr
    Just (kw, cond) -> return $ IfStmt kw cond expr Nothing

-- Check tlCallExpr here before anything else so that f[1] parses as access, not
-- a call on a list
toplevelExpr :: EParser Expr
toplevelExpr = try tlCallExpr <|> try funcSyntax <|> try callSyntax <|> basicExpr
    where funcSyntax = do
            name <- identifier
            -- If followed by open paren, make sure there are multiple args.
            -- If there aren't, this should be parsed as a call chain, not a top-
            -- level
            paren <- option False $ True <$ lookAhead (operator "(")
            -- If followed by plus or minus, parse as operator expression, not top-
            -- level.
            notFollowedBy $ operator "+" <|> operator "-"
            firstArg <- basicExpr
            restArgs <- many (nlPrecedingComma *> basicExpr)
            when (null restArgs && paren) $
                 fail "top paren error; if you see this message, please report it"
            return $ FunctionCall (Ident name) (firstArg : restArgs)
          callSyntax = do
            call <- callLHS
            operator "."
            newlines
            func <- funcSyntax
            case func of
              FunctionCall (Ident expr) args -> return $ DotCall call expr args
              _ -> fail "call syntax fail error; report this message if you see it"
          nlPrecedingComma = operator "," <* newlines

basicExpr :: EParser Expr
basicExpr = Oper <$> operatorExpr basicTerm <?> "basic expression"

basicTerm :: EParser Expr
basicTerm = beginEndExpr <|> ifExpr <|> forExpr <|> caseExpr <|> condExpr <|>
            parenExpr <|> literalExpr <|> tupleExpr <|> listExpr <|>
            try (Declare <$> varDecl) <|> try (Declare <$> functionDecl) <|>
            try varAsn <|> try callExpr

beginEndExpr :: EParser Expr
beginEndExpr = do
  keyword "begin"
  newlines1
  stmts <- endBy statement newlines1
  keyword "end"
  return $ Block stmts

ifExpr :: EParser Expr
ifExpr = do
  kw <- If <$ keyword "if" <|> Unless <$ keyword "unless"
  cond <- toplevelExpr
  (true, false) <- oneLine <|> multiLine
  return $ IfStmt kw cond true false
    where oneLine = do
            keyword "then"
            newlines
            true <- toplevelExpr
            newlines
            keyword "else"
            newlines
            false <- toplevelExpr
            return (true, Just false)
          multiLine = do
                      newlines1
                      true <- many (statement <* newlines1)
                      false <- optionMaybe $ do
                                 keyword "else"
                                 newlines1
                                 many (statement <* newlines1)
                      keyword "end"
                      return (Block $ true, Block <$> false)

caseExpr :: EParser Expr
caseExpr = fail "Not implemented; case"

condExpr :: EParser Expr
condExpr = fail "Not implemented; cond"

parenExpr :: EParser Expr
parenExpr = operator "(" *> newlines *>
            statement
            <* newlines <* operator ")"

literalExpr :: EParser Expr
literalExpr = do
  lex <- satisfy $ \x -> case x of
                           Left y -> case y of
                                       ReMatch {} -> True
                                       ReSub {} -> True
                                       Number {} -> True
                                       Character {} -> True
                                       String {} -> True
                                       Symbol {} -> True
                                       _ -> False
                           Right _ -> False
  case lex of
    Left x -> return $ Literal x
    Right _ -> unexpected "newline"

tupleExpr :: EParser Expr
tupleExpr = do
  operator "{"
  newlines
  contents <- sepBy basicExpr nlComma
  newlines
  operator "}"
  return $ TupleExpr contents

listExpr :: EParser Expr
listExpr = do
  operator "["
  newlines
  contents <- sepBy basicExpr nlComma
  newlines
  operator "]"
  return $ ListExpr contents

varDecl :: EParser Decl
varDecl = do
  type_ <- option Nothing $ try (Just <$> typeSpec <* newlines1)
  keyword "var"
  newlines
  name <- identifier <?> "variable name"
  type' <- case type_ of
             Nothing -> return Nothing
             Just (str, expr)
                 | str == name -> return $ Just expr
                 | otherwise -> fail "preceding type declaration should match function"
  newlines
  operator "="
  newlines
  stmt <- statement
  return $ Variable type' name stmt

varAsn :: EParser Expr
varAsn = do
  name <- callExpr
  op <- operator "=" <|> operator "+=" <|> operator "-=" <|>
        operator "*=" <|> operator "/=" <|> operator "&&=" <|>
        operator "||="
  newlines
  expr <- toplevelExpr
  let (name', expr') =
          case op of
            Left (Operator "=") -> (name, expr)
            Left (Operator "+=") -> (name, Oper (Inf (OpExpr name) Plus (OpExpr expr)))
            Left (Operator "-=") -> (name, Oper (Inf (OpExpr name) Minus (OpExpr expr)))
            Left (Operator "*=") -> (name, Oper (Inf (OpExpr name) Times (OpExpr expr)))
            Left (Operator "/=") -> (name, Oper (Inf (OpExpr name) Div (OpExpr expr)))
            Left (Operator "&&=") -> (name, Oper (Inf (OpExpr name)
                                                      UniversalAnd (OpExpr expr)))
            Left (Operator "||=") -> (name, Oper (Inf (OpExpr name)
                                                      UniversalOr (OpExpr expr)))
  case name' of
    Ident x -> return $ VarAsn x expr'
    DotCall lhs name' rhs -> return $ DotCall lhs (name' ++ "_asn") (expr' : rhs)
    _ -> fail "variable assignment syntax failed; report this message"

callLHS :: EParser Expr
callLHS = Ident <$> identifier <|> operator "(" *> toplevelExpr <* operator ")"

callExpr :: EParser Expr
callExpr = do
  name <- callLHS
  chain <- many $ parenCall <|> bracketCall <|> dotCall
  return $ foldl foldFunc name chain
      where parenCall = do
              operator "("
              newlines
              args <- sepBy basicExpr nlComma
              newlines
              operator ")"
              return (Paren, args)
            bracketCall = do
              operator "["
              newlines
              args <- sepBy basicExpr nlComma
              newlines
              operator "]"
              return (Bracket, args)
            dotCall = do
              operator "."
              newlines
              funcName <- identifier
              args <- option [] $ do
                          operator "("
                          newlines
                          args' <- sepBy basicExpr nlComma
                          newlines
                          operator ")"
                          return args'
              return (Dot funcName, args)
            foldFunc expr (Paren, args) = FunctionCall expr args
            foldFunc expr (Bracket, args) = Subscript expr args
            foldFunc expr (Dot name, args) = DotCall expr name args

-- A rather bizarre workaround for non-ambiguity's sake. See toplevelExpr for details.
tlCallExpr :: EParser Expr
tlCallExpr = lookAhead (identifier >> operator "[") >> callExpr

nlComma :: EParser ()
nlComma = void $ newlines *> operator "," <* newlines
