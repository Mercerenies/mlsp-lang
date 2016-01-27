module Lang.Parser(FileData(..), Decl(..), Type(..), TypeExpr(..), Context(..), Expr(..),
                   Conditional(..), Pattern(..), Literal(..), FunctionDecl(..),
                   ClassDecl(..), Access(..), IfOp(..), ForOp(..), Call(..), MetaCall(..),
                   parseCode, file, toplevel) where

-- TODO Consider making SourcePos a monad rather than a property of EVERYTHING

import Lang.Tokens
import Lang.Operator
import Lang.Identifier
import Data.List(intercalate)
import Data.Either(partitionEithers)
import Data.Maybe(isJust)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Error(ParseError)
import Text.Parsec.Pos(SourcePos)
import Control.Monad

data FileData = FileData String [Decl] -- Package, declarations
                deriving (Show, Eq)

type FunctionBody = [([Pattern], Expr)]

-- TODO Do something useful with modules to make them more than just namespaces

-- TODO With the new 'def' syntax, we might be able to support some symbols in
--      method names specifically (x.var=, x.[], etc.)

{-
Import, Function, TypeDecl, Class, Concept, Instance, Generic, Meta
ValueId = Function | TypeDecl | Class | Concept | Generic
SymbolTable [Import] (Map String ValueId) (Map String TypeDecl) (Map String Meta)
-}

data Decl = Import SourcePos String [String] | -- Name, hiding
            Include SourcePos String [String] | -- Name, hiding
            Module SourcePos String [Decl] |
            Function SourcePos FunctionDecl |
            TypeDecl SourcePos String [String] TypeExpr |
            -- Name, arguments, parent, children, abstract, variables, methods
            Class SourcePos String [String] TypeExpr (Maybe [TypeExpr]) Bool [ClassDecl] |
            -- Name, args, variables
            Concept SourcePos String [String] Context [(String, Type)] |
            Instance SourcePos String [TypeExpr] Context [FunctionDecl] |
            Generic SourcePos String Type |
            Meta SourcePos FunctionDecl |
            MetaDeclare SourcePos MetaCall
            deriving (Show, Eq)

data ClassDecl = Field SourcePos String TypeExpr |
                 Method SourcePos FunctionDecl |
                 MetaDeclClass SourcePos MetaCall
                 deriving (Show, Eq)

data FunctionDecl = FunctionDecl (Maybe Type) String FunctionBody
                    deriving (Show, Eq)

data Type = Type TypeExpr Context
            deriving (Show, Eq)

-- The type expressions should evaluate to contexts
newtype Context = Context [TypeExpr]
    deriving (Show, Eq)

data TypeExpr = TypeOper SourcePos (OpExpr TypeOp TypeExpr) |
                Tuple SourcePos [TypeExpr] Access |
                Named SourcePos String [TypeExpr] Access |
                Func SourcePos [TypeExpr] TypeExpr
                deriving (Show, Eq)

data Expr = FunctionCall SourcePos Expr [Expr] |
            DotCall SourcePos Expr String [Expr] |
            Block SourcePos [Expr] |
            Literal SourcePos Literal |
            TupleExpr SourcePos [Expr] |
            ListExpr SourcePos [Expr] |
            VarAsn SourcePos Pattern Expr |
            Subscript SourcePos Expr [Expr] |
            Ident SourcePos String |
            Oper SourcePos (OpExpr Op Expr) |
            IfStmt SourcePos IfOp Conditional Expr (Maybe Expr) |
            ForStmt SourcePos ForOp Pattern Expr Expr |
            Case SourcePos Expr [(Pattern, Maybe Expr, Expr)] |
            Cond SourcePos [(Conditional, Expr)] (Maybe Expr) |
            -- Vars, Funcs, Expr
            LetStmt SourcePos [(Pattern, Expr)] [(String, Maybe Type, FunctionBody)] Expr |
            Lambda SourcePos [String] Expr |
            MetaExpr SourcePos MetaCall
            deriving (Show, Eq)

data Literal = LReMatch String |
               LReSub String String |
               LNumber Integer String Integer |
               LCharacter Char |
               LString [TextToken] |
               LSymbol String
               deriving (Show, Read, Eq)

data Pattern = TuplePattern SourcePos [Pattern] |
               ListPattern SourcePos [Pattern] |
               SplatPattern SourcePos [Pattern] String [Pattern] |
               IdPattern SourcePos String |
               TypePattern SourcePos String [Pattern] |
               ExprPattern SourcePos Expr |
               UnderscorePattern SourcePos |
               MetaPattern SourcePos MetaCall
               deriving (Show, Eq)

data MetaCall = MetaCall String [Token]
                deriving (Show, Eq)

data MetaSpecial = Parent TypeExpr |
                   Children [TypeExpr] |
                   Abstract
                   deriving (Show, Eq)

data Conditional = CondExpr Expr |
                   BindExpr Pattern Expr
                   deriving (Show, Eq)

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

parseCode :: String -> [Lexeme] -> Either ParseError FileData
parseCode = parse file

file :: EParser FileData
file = do
  newlines
  let pkgName = fromPackageName mainPackageName
  pkg <- option pkgName $ keyword "package" *> dottedIdentifier <* newlines1
  decl <- endBy toplevel newlines1
  eof
  return $ FileData pkg decl

toplevel :: EParser Decl
toplevel = moduleDecl <|> functionDecl <|> typeDecl <|>
           conceptDecl <|> instanceDecl <|> importInclude <|>
           classDecl <|> genericDecl <|> metaDecl <|> metaDeclCall

importInclude :: EParser Decl
importInclude = do
  constr <- Import <$ keyword "import" <|> Include <$ keyword "include"
  name <- dottedIdentifier
  hiding <- option [] $ do
                  keyword "hiding"
                  newlines
                  operator "("
                  newlines
                  inside <- sepBy identifier nlComma
                  newlines
                  operator ")"
                  return inside
  pos <- getPosition
  return $ constr pos name hiding

-- TODO Make include and module do something

moduleDecl :: EParser Decl
moduleDecl = do
  keyword "module"
  newlines
  name <- identifier
  newlines1
  contents <- many (toplevel <* newlines1)
  keyword "end"
  pos <- getPosition
  return $ Module pos name contents

genericDecl :: EParser Decl
genericDecl = do
  keyword "generic"
  newlines
  name <- identifier
  newlines
  operator "::"
  newlines
  expr <- typeAndContext
  pos <- getPosition
  return $ Generic pos name expr

metaDeclCall :: EParser Decl
metaDeclCall = do
  mc <- metaCall
  pos <- getPosition
  case mc of
    Left x -> return $ MetaDeclare pos x
    Right (Parent {}) -> unexpected "parent declaration" <?> "meta call"
    Right (Children {}) -> unexpected "children declaration" <?> "meta call"
    Right (Abstract {}) -> unexpected "abstract declaration" <?> "meta call"

metaDecl :: EParser Decl
metaDecl = do
  keyword "meta"
  newlines
  Meta <$> getPosition <*> functionDecl'

functionDecl :: EParser Decl
functionDecl = do
  keyword "def"
  newlines
  Function <$> getPosition <*> functionDecl'

functionDecl' :: EParser FunctionDecl
functionDecl' = do
  name <- dottedIdentifier
  operator "("
  newlines
  args <- sepBy pattern nlComma
  newlines
  operator ")"
  type_ <- optionMaybe $ operator "::" *> newlines *> typeAndContext
  contents <- Left <$> funcShortForm <|> Right <$> funcLongForm
  case contents of
    Left stmt -> return $ FunctionDecl type_ name [(args, stmt)]
    Right insides ->
        let getIdPattern (IdPattern _ x) = Just x
            getIdPattern _ = Nothing
            args' = mapM getIdPattern args
        in case args' of
             Nothing -> fail "invalid pattern in function long form"
             Just args''
                 | null insides -> fail "empty function declaration"
                 | length args'' /= length (fst $ head insides) ->
                     fail "incompatible arglist in function declaration"
                 | otherwise -> return $ FunctionDecl type_ name insides

funcShortForm :: EParser Expr
funcShortForm = operator "=" *> newlines *> statement

funcLongForm :: EParser [([Pattern], Expr)]
funcLongForm = newlines1 *> many (singleForm <* newlines1) <* keyword "end"
    where singleForm = do
            operator "("
            newlines
            args <- sepBy pattern nlComma
            newlines
            operator ")"
            operator "->"
            stmt <- statement
            return (args, stmt)

typeDecl :: EParser Decl
typeDecl = do
  keyword "type"
  newlines
  name <- identifier
  args <- option [] $ operator "[" *> sepBy identifier nlComma <* operator "]"
  newlines
  operator "="
  newlines
  synonym <- typeExpr
  pos <- getPosition
  return $ TypeDecl pos name args synonym

classDecl :: EParser Decl
classDecl = do
  keyword "class"
  newlines
  name <- identifier
  args <- option [] $ operator "[" *> sepBy identifier nlComma <* operator "]"
  pos0 <- getPosition
  newlines1
  contents <- many $ (classMethod <|> classMeta <|> classField) <* newlines1
  let (inner, parent, children, abstract) = foldl process init contents
      parent' = maybe (Named pos0 "T" [] Read) id parent
  keyword "end"
  pos <- getPosition
  return $ Class pos name args parent' children abstract (inner [])
      where classField = do
              name <- identifier
              operator "::"
              newlines
              type_ <- typeExpr
              pos <- getPosition
              return $ Left (Field pos name type_)
            classMethod = do
                           keyword "def"
                           newlines
                           decl <- functionDecl'
                           pos <- getPosition
                           return $ Left (Method pos decl)
            classMeta = metaCall >>= \mc ->
                        case mc of
                          Left call -> (\x -> Left $ MetaDeclClass x call) <$> getPosition
                          Right (Parent expr) -> return . Right $ Parent expr
                          Right (Children expr) -> return . Right $ Children expr
                          Right Abstract -> return $ Right Abstract
            init = (id, Nothing, Nothing, False)
            process (a, _, c, d) (Right (Parent expr)) = (a, Just expr, c, d)
            process (a, b, _, d) (Right (Children expr)) = (a, b, Just expr, d)
            process (a, b, c, _) (Right Abstract) = (a, b, c, True)
            process (a, b, c, d) (Left decl) = (a . (decl :), b, c, d)

typeSpec :: EParser (String, Type)
typeSpec = do
  name <- identifier
  operator "::"
  newlines
  tpe <- typeAndContext
  return (name, tpe)

typeAndContext :: EParser Type
typeAndContext = Type <$> typeExpr <*> option idContext (try $ newlines *> contextExpr)

contextExpr :: EParser Context
contextExpr = do
  keyword "where"
  newlines
  Context <$> (shortContext <|> longContext)
      where shortContext = return <$> typeExpr
            longContext = do
                operator "("
                newlines
                inner <- sepBy typeExpr nlComma
                newlines
                operator ")"
                return inner

typeExpr :: EParser TypeExpr
typeExpr = TypeOper <$> getPosition <*> typeOperatorExpr typeTerm

typeTerm :: EParser TypeExpr
typeTerm = try funcTypeExpr <|> try tupleTypeExpr <|> namedTypeExpr <?> "type expression"

-- NOTE: If the length ends up being one, it will return the inner type, NOT a one-tuple.
tupleTypeExpr :: EParser TypeExpr
tupleTypeExpr = do
  operator "("
  newlines
  contents <- sepBy typeExpr nlComma
  newlines
  operator ")"
  case contents of
    [single] -> return single -- Simple grouping parens
    _ -> do
      access <- accessSuffix
      pos <- getPosition
      return $ Tuple pos contents access

namedTypeExpr :: EParser TypeExpr
namedTypeExpr = do
  name <- dottedIdentifier
  args <- option [] $ do
                  operator "["
                  newlines
                  args' <- sepBy typeExpr nlComma
                  newlines
                  operator "]"
                  return args'
  access <- accessSuffix
  pos <- getPosition
  return $ Named pos name args access

funcTypeExpr :: EParser TypeExpr
funcTypeExpr = do
  operator "("
  newlines
  lhs <- sepBy typeExpr nlComma
  newlines
  operator ")"
  operator "->"
  newlines
  rhs <- typeExpr
  pos <- getPosition
  return $ Func pos lhs rhs

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
  context <- option idContext $ try (newlines *> contextExpr)
  newlines1
  internals <- endBy typeSpec newlines1
  keyword "end"
  pos <- getPosition
  return $ Concept pos name args context internals

instanceDecl :: EParser Decl
instanceDecl = do
  keyword "instance"
  newlines
  name <- dottedIdentifier
  args <- option [] $ do
                operator "["
                newlines
                args' <- sepBy typeExpr nlComma
                newlines
                operator "]"
                return args'
  context <- option idContext $ try (newlines *> contextExpr)
  newlines1
  internals <- endBy (keyword "def" *> newlines *> functionDecl') newlines1
  keyword "end"
  pos <- getPosition
  return $ Instance pos name args context internals

statement :: EParser Expr
statement = do
  expr <- toplevelExpr
  op <- optionMaybe $ do
              op' <- If <$ keyword "if" <|> Unless <$ keyword "unless"
              cond <- condit
              return (op', cond)
  pos <- getPosition
  case op of
    Nothing -> return expr
    Just (kw, cond) -> return $ IfStmt pos kw cond expr Nothing

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
            pos <- getPosition
            return $ FunctionCall pos (Ident pos name) (firstArg : restArgs)
          callSyntax = do
            call <- callLHS
            operator "."
            newlines
            func <- funcSyntax
            case func of
              FunctionCall pos (Ident _ expr) args ->
                  return $ DotCall pos call expr args
              _ ->
                  fail "call syntax fail error; report this message if you see it"
          nlPrecedingComma = operator "," <* newlines

basicExpr :: EParser Expr
basicExpr = Oper <$> getPosition <*> operatorExpr basicTerm <?> "basic expression"

basicTerm :: EParser Expr
basicTerm = beginEndExpr <|> ifExpr <|> forExpr <|> caseExpr <|> condExpr <|>
            letExpr <|> lambdaExpr <|> try varAsn <|> metaExpr <|>
            literalExpr <|> listExpr <|> try tupleExpr <|>
            parenExpr <|> try callExpr

metaExpr :: EParser Expr
metaExpr = do
  mc <- metaCall
  pos <- getPosition
  case mc of
    Left x -> return $ MetaExpr pos x
    Right (Parent {}) -> unexpected "parent declaration" <?> "meta call"
    Right (Children {}) -> unexpected "children declaration" <?> "meta call"
    Right (Abstract {}) -> unexpected "abstract declaration" <?> "meta call"

beginEndExpr :: EParser Expr
beginEndExpr = do
  keyword "begin"
  newlines1
  stmts <- endBy statement newlines1
  keyword "end"
  pos <- getPosition
  return $ Block pos stmts

ifExpr :: EParser Expr
ifExpr = do
  kw <- If <$ keyword "if" <|> Unless <$ keyword "unless"
  newlines
  cond <- condit
  (true, false) <- oneLine <|> multiLine
  pos <- getPosition
  return $ IfStmt pos kw cond true false
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
                      pos <- getPosition
                      return (Block pos true, Block pos <$> false)

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
  pos <- getPosition
  return $ ForStmt pos op ptn expr inner
      where oneLine = do
              keyword "then"
              newlines
              toplevelExpr
            multiLine = do
                        newlines1
                        expr <- many (statement <* newlines1)
                        keyword "end"
                        pos <- getPosition
                        return $ Block pos expr

caseExpr :: EParser Expr
caseExpr = do
  keyword "case"
  newlines
  expr <- toplevelExpr
  newlines1
  clauses <- many (clause <* newlines1)
  keyword "end"
  pos <- getPosition
  return $ Case pos expr clauses
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
          multiLine = Block <$> getPosition <*> many (try $ newlines1 *> statement)

condExpr :: EParser Expr
condExpr = do
  keyword "cond"
  newlines
  clauses <- many (clause <* newlines1)
  else_ <- optionMaybe $ do
             keyword "else"
             newlines
             Block <$> getPosition <*> many (statement <* newlines1)
  keyword "end"
  pos <- getPosition
  return $ Cond pos clauses else_
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
          multiLine = Block <$> getPosition <*> many (try $ newlines1 *> statement)

letExpr :: EParser Expr
letExpr = do
  keyword "let"
  newlines
  -- TODO Optimize out this 'try' without exploding everything
  clauses <- (:) <$> clause <*> many (try $ newlines1 *> clause)
  let (vars, funcs) = partitionEithers clauses
  newlines
  keyword "in"
  newlines
  expr <- toplevelExpr
  pos <- getPosition
  return $ LetStmt pos vars funcs expr
    where clause = Left <$> try varClause <|> Right <$> try funcClause
          -- TODO Factor out the 'try's in 'clause'
          varClause = do
            ident <- pattern
            newlines
            operator "="
            newlines
            expr <- statement
            return (ident, expr)
          funcClause = do
            FunctionDecl type_ name clauses <- functionDecl'
            return (name, type_, clauses)

-- TODO Lambdas and function declarations should be able to pattern match their
--      arguments.
lambdaExpr :: EParser Expr
lambdaExpr = do
  operator "->"
  newlines
  args <- option [] $ do
               operator "("
               newlines
               args' <- sepBy identifier nlComma
               newlines
               operator ")"
               return args'
  newlines
  expr <- toplevelExpr
  pos <- getPosition
  return $ Lambda pos args expr

parenExpr :: EParser Expr
parenExpr = operator "(" *> newlines *>
            statement
            <* newlines <* operator ")"

literalExpr :: EParser Expr
literalExpr = do
  lex <- satisfy $ isJust . literalize
  pos <- getPosition
  case literalize lex of
    Just x -> return $ Literal pos x
    _ -> unexpected (show lex) <?> "literal"
 where literalize (Token y _) = case y of
                                  ReMatch s -> Just $ LReMatch s
                                  ReSub a b -> Just $ LReSub a b
                                  Number n d e -> Just $ LNumber n d e
                                  Character c -> Just $ LCharacter c
                                  String s -> Just $ LString s
                                  Symbol s -> Just $ LSymbol s
                                  _ -> Nothing
       literalize _ = Nothing

tupleExpr :: EParser Expr
tupleExpr = do
  operator "("
  newlines
  contents <- sepBy basicExpr nlComma
  newlines
  operator ")"
  when (length contents == 1) $
       fail "tuple of length 1"
  pos <- getPosition
  return $ TupleExpr pos contents

listExpr :: EParser Expr
listExpr = do
  operator "["
  newlines
  contents <- sepBy basicExpr nlComma
  newlines
  operator "]"
  pos <- getPosition
  return $ ListExpr pos contents

varAsn :: EParser Expr
varAsn = do
  name <- Right <$> callExpr <|> Left <$> try pattern
  op <- operator "=" <|> operator "+=" <|> operator "-=" <|>
        operator "*=" <|> operator "/=" <|> operator "&&=" <|>
        operator "||="
  newlines
  expr <- toplevelExpr
  pos0 <- getPosition
  (name', expr') <-
      case name of
        Right name ->
            case op of
              Token (Operator "=") _ ->
                  return (Right name, expr)
              Token (Operator "+=") _ ->
                  return (Right name, Oper pos0 (Inf (OpExpr name) Plus (OpExpr expr)))
              Token (Operator "-=") _ ->
                  return (Right name, Oper pos0 (Inf (OpExpr name) Minus (OpExpr expr)))
              Token (Operator "*=") _ ->
                  return (Right name, Oper pos0 (Inf (OpExpr name) Times (OpExpr expr)))
              Token (Operator "/=") _ ->
                  return (Right name, Oper pos0 (Inf (OpExpr name) Div (OpExpr expr)))
              Token (Operator "&&=") _ ->
                  return (Right name, Oper pos0
                                    (Inf (OpExpr name) UniversalAnd (OpExpr expr)))
              Token (Operator "||=") _ ->
                  return (Right name, Oper pos0
                                    (Inf (OpExpr name) UniversalOr (OpExpr expr)))
              _ -> fail "variable assignment operator failed; report this message"
        Left _ ->
            case op of
              Token (Operator "=") _ -> return (name, expr)
              _ -> unexpected "compound assignment on pattern"
  pos <- getPosition
  case name' of
    Left pat ->
        return $ VarAsn pos pat expr'
    Right (Ident pos' x) ->
        return $ VarAsn pos (IdPattern pos' x) expr'
    Right (DotCall _ lhs name' rhs) ->
        return $ DotCall pos lhs (name' ++ "_asn") (expr' : rhs)
    _ ->
        fail "variable assignment syntax failed; report this message"

callLHS :: EParser Expr
callLHS = Ident <$> getPosition <*> identifier <|>
          operator "(" *> toplevelExpr <* operator ")"

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
              pos <- getPosition
              return (Paren, pos, args)
            bracketCall = do
              operator "["
              newlines
              args <- sepBy basicExpr nlComma
              newlines
              operator "]"
              pos <- getPosition
              return (Bracket, pos, args)
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
              pos <- getPosition
              return (Dot funcName, pos, args)
            foldFunc expr (Paren, pos, args) = FunctionCall pos expr args
            foldFunc expr (Bracket, pos, args) = Subscript pos expr args
            foldFunc expr (Dot name, pos, args) = DotCall pos expr name args

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

-- TODO Try literal patterns (so numbers, etc. don't have to be written
--      with a caret (^) preceding them)
pattern :: EParser Pattern
pattern = try tuplePattern <|> listPattern <|> exprPattern <|>
          UnderscorePattern <$> (matchToken (Identifier "_") *> getPosition) <|>
          try typePattern <|> metaPattern <|> idPattern <?> "pattern"

metaPattern :: EParser Pattern
metaPattern = do
  mc <- metaCall
  pos <- getPosition
  case mc of
    Left x -> return $ MetaPattern pos x
    Right (Parent {}) -> unexpected "parent declaration" <?> "meta call"
    Right (Children {}) -> unexpected "children declaration" <?> "meta call"
    Right (Abstract {}) -> unexpected "abstract declaration" <?> "meta call"

tuplePattern :: EParser Pattern
tuplePattern = do
  operator "("
  newlines
  contents <- sepBy pattern nlComma
  newlines
  operator ")"
  when (length contents == 1) $
       fail "tuple of length 1"
  pos <- getPosition
  return $ TuplePattern pos contents

listPattern :: EParser Pattern
listPattern = do
  operator "[" >> newlines
  args <- sepBy pattern nlComma
  pos0 <- getPosition
  result <- option (ListPattern pos0 args) $ do
                operator "..."
                rest <- many $ nlComma *>  pattern
                case args of
                  [] -> unexpected "ellipsis"
                  _ -> case last args of
                         IdPattern pos pat ->
                             return $ SplatPattern pos (init args) pat rest
                         _ ->
                             unexpected "pattern" <?> "identifier"
  newlines >> operator "]"
  return result

exprPattern :: EParser Pattern
exprPattern = ExprPattern <$> getPosition <*> (operator "^" *> newlines *> toplevelExpr)

typePattern :: EParser Pattern
typePattern = do
  operator "&" -- TODO Find a way (without 'try's everywhere) to remove this ampersand
  newlines
  name <- identifier
  newlines
  operator "["
  newlines
  args <- sepBy pattern nlComma
  newlines
  operator "]"
  pos <- getPosition
  return $ TypePattern pos name args

idPattern :: EParser Pattern
idPattern = IdPattern <$> getPosition <*> identifier

-- TODO Desugar this immediately
accessSuffix :: EParser Access
accessSuffix = option Read $ ReadWrite <$ operator "!"

metaCall :: EParser (Either MetaCall MetaSpecial)
metaCall = do
  operator "{"
  newlines
  next <- anyToken
  result <- case next of
              Token (Identifier "parent") _ ->
                  (Right . Parent) <$> (newlines *> typeExpr)
              Token (Identifier "children") _ ->
                  (Right . Children) <$> (newlines *> sepBy typeExpr nlComma)
              Token (Identifier "abstract") _ ->
                  pure $ Right Abstract
              Token (Identifier str) _ -> Left <$> inner str
              _ -> unexpected (show next) <?> "meta identifier"
  operator "}"
  return result
   where inner name = MetaCall name <$> option [] readArgs
         readArgs = do
              next <- satisfy $ \x -> case x of
                                        Token (Operator "}") _ -> False
                                        _ -> True
              case next of
                Token (Operator "{") _ -> do
                          inside <- option [] readArgs
                          operator "}"
                          outside <- option [] readArgs
                          return $ [Operator "{"] ++ inside ++ [Operator "}"] ++ outside
                Token x _ -> (x :) <$> option [] readArgs
                Newline -> option [] readArgs

nlComma :: EParser ()
nlComma = void $ operator "," <* newlines

dottedIdentifier :: EParser String
dottedIdentifier = do
  first <- identifier
  rest <- many $ operator "." *> identifier
  return $ intercalate "." (first : rest)

idContext :: Context
idContext = Context []
