module Lang.Parser(Decl(..), Type(..), Expr(..), Conditional(..), Pattern(..),
                   Fields(..), Access(..), IfOp(..), ForOp(..), Call(..), Timing(..),
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
import Control.Monad

data Decl = Module String [Decl] |
            Function (Maybe Type) String [String] Expr |
            Type String Type [(String, Type)] Fields | -- Name, parent, variables, fields
            Concept String [String] Timing [(String, Type)] | -- Name, args, variables
            Instance String [Type] Type [Decl] |
            Variable (Maybe Type) String Expr
            deriving (Show, Read, Eq)

data Type = Tuple [Type] Access |
            Named String [Type] Access |
            Func [Type] Type
            deriving (Show, Read, Eq)

data Expr = FunctionCall Expr [Expr] |
            DotCall Expr String [Expr] |
            Block [Expr] |
            Literal Token |
            TupleExpr [Expr] |
            ListExpr [Expr] |
            Declare Decl |
            VarAsn Pattern Expr |
            Subscript Expr [Expr] |
            Ident String |
            Oper (OpExpr Expr) |
            IfStmt IfOp Conditional Expr (Maybe Expr) |
            ForStmt ForOp Pattern Expr Expr |
            Case Expr [(Pattern, Maybe Expr, Expr)] |
            Cond [(Conditional, Expr)] (Maybe Expr)
            deriving (Show, Read, Eq)

data Pattern = TuplePattern [Pattern] |
               ListPattern [Pattern] |
               SplatPattern [Pattern] String [Pattern] |
               IdPattern String |
               TypePattern String [Pattern] |
               ExprPattern Expr |
               UnderscorePattern
               deriving (Show, Read, Eq)

data Conditional = CondExpr Expr |
                   BindExpr Pattern Expr
                   deriving (Show, Read, Eq)

newtype Fields = Fields (Map String Access)
    deriving (Show, Read, Eq)

data Access = Read | ReadWrite
              deriving (Show, Read, Eq, Ord)

data IfOp = If | Unless
            deriving (Show, Read, Eq, Ord)

data ForOp = ForEq | ForBind
             deriving (Show, Read, Eq, Ord)

data Call = Paren | Bracket | Dot String
            deriving (Show, Read, Eq, Ord)

data Timing = Static | Dynamic
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
  parent <- option (Named "T" [] Read) $ operator "(" *> typeExpr <* operator ")"
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
    where identifier' = do
            result <- identifier
            opt <- accessSuffix
            return (result, opt)

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
  access <- accessSuffix
  return $ Tuple contents access

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
  access <- accessSuffix
  return $ Named name args access

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
  binding <- option Static $ Dynamic <$ keyword "dynamic" <* newlines
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
  return $ Concept name args binding internals

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
              cond <- condit
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
            parenExpr <|> try varAsn <|> literalExpr <|> tupleExpr <|> listExpr <|>
            try (Declare <$> varDecl) <|> try (Declare <$> functionDecl) <|>
            try callExpr

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
  newlines
  cond <- condit
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

-- ///// For and Case

forExpr :: EParser Expr
forExpr = do
  keyword "for"
  newlines
  ptn <- pattern
  newlines
  op <- ForEq <$ operator "=" <|> ForBind <$ operator "<-"
  newlines
  expr <- toplevelExpr
  inner <- oneLine <|> multiLine
  return $ ForStmt op ptn expr inner
      where oneLine = do
              keyword "then"
              newlines
              toplevelExpr
            multiLine = do
                        newlines1
                        expr <- many (statement <* newlines1)
                        keyword "end"
                        return $ Block expr

caseExpr :: EParser Expr
caseExpr = do
  keyword "case"
  newlines
  expr <- toplevelExpr
  newlines1
  clauses <- many (clause <* newlines1)
  keyword "end"
  return $ Case expr clauses
    where clause = do
             keyword "when"
             newlines
             cond <- pattern
             guardClause <- optionMaybe $ do
                              operator "|" <?> "guard clause"
                              newlines
                              basicExpr
             body <- singleLine <|> multiLine
             return (cond, guardClause, body)
          singleLine = do
                     keyword "then"
                     newlines
                     toplevelExpr
          multiLine = Block <$> many (try $ newlines1 *> statement)

condExpr :: EParser Expr
condExpr = do
  keyword "cond"
  newlines
  clauses <- many (clause <* newlines1)
  else_ <- optionMaybe $ do
             keyword "else"
             newlines
             Block <$> many (statement <* newlines1)
  keyword "end"
  return $ Cond clauses else_
    where clause = do
             keyword "when"
             newlines
             cond <- condit
             body <- singleLine <|> multiLine
             return (cond, body)
          singleLine = do
                     keyword "then"
                     newlines
                     toplevelExpr
          multiLine = Block <$> many (try $ newlines1 *> statement)

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
  name <- Left <$> try pattern <|> Right <$> callExpr
  op <- operator "=" <|> operator "+=" <|> operator "-=" <|>
        operator "*=" <|> operator "/=" <|> operator "&&=" <|>
        operator "||="
  newlines
  expr <- toplevelExpr
  (name', expr') <-
      case name of
        Right name ->
            case op of
              Left (Operator "=") ->
                  return (Right name, expr)
              Left (Operator "+=") ->
                  return (Right name, Oper (Inf (OpExpr name) Plus (OpExpr expr)))
              Left (Operator "-=") ->
                  return (Right name, Oper (Inf (OpExpr name) Minus (OpExpr expr)))
              Left (Operator "*=") ->
                  return (Right name, Oper (Inf (OpExpr name) Times (OpExpr expr)))
              Left (Operator "/=") ->
                  return (Right name, Oper (Inf (OpExpr name) Div (OpExpr expr)))
              Left (Operator "&&=") ->
                  return (Right name, Oper (Inf (OpExpr name) UniversalAnd (OpExpr expr)))
              Left (Operator "||=") ->
                  return (Right name, Oper (Inf (OpExpr name) UniversalOr (OpExpr expr)))
              _ -> fail "variable assignment operator failed; report this message"
        Left _ ->
            case op of
              Left (Operator "=") -> return (name, expr)
              _ -> unexpected "compound assignment on pattern"
  case name' of
    Left pat -> return $ VarAsn pat expr'
    Right (Ident x) -> return $ VarAsn (IdPattern x) expr'
    Right (DotCall lhs name' rhs) -> return $ DotCall lhs (name' ++ "_asn") (expr' : rhs)
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
tlCallExpr = do
  lookAhead (identifier >> operator "[")
  callExpr

condit :: EParser Conditional
condit = try bindExpr <|> CondExpr <$> toplevelExpr
    where bindExpr = do
            lhs <- pattern
            operator "<-"
            newlines
            expr <- toplevelExpr
            return $ BindExpr lhs expr

pattern :: EParser Pattern
pattern = tuplePattern <|> listPattern <|> exprPattern <|>
          UnderscorePattern <$ matchToken (Identifier "_") <|>
          try typePattern <|> idPattern <?> "pattern"

tuplePattern :: EParser Pattern
tuplePattern = operator "{" *> newlines *>
               (TuplePattern <$> sepBy pattern nlComma)
               <* newlines <* operator "}"

listPattern :: EParser Pattern
listPattern = do
  operator "[" >> newlines
  args <- sepBy pattern nlComma
  result <- option (ListPattern args) $ do
                operator "..."
                rest <- many $ nlComma *>  pattern
                case args of
                  [] -> unexpected "ellipsis"
                  _ -> case last args of
                         IdPattern pat -> return $ SplatPattern (init args) pat rest
                         _ -> unexpected "pattern" <?> "identifier"
  newlines >> operator "]"
  return result

exprPattern :: EParser Pattern
exprPattern = ExprPattern <$> (operator "^" *> newlines *> toplevelExpr)

typePattern :: EParser Pattern
typePattern = do
  operator "&"
  newlines
  name <- identifier
  newlines
  operator "["
  newlines
  args <- sepBy pattern nlComma
  newlines
  operator "]"
  return $ TypePattern name args

idPattern :: EParser Pattern
idPattern = IdPattern <$> identifier

accessSuffix :: EParser Access
accessSuffix = option Read $ ReadWrite <$ operator "!"

nlComma :: EParser ()
nlComma = void $ newlines *> operator "," <* newlines