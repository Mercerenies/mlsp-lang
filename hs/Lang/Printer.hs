module Lang.Printer(Output(..), SExpr(..), Lispable(..),
                    showSexp, printSexp, output, maybeToOutput) where

-- TODO Alter the syntax slightly to allow default implementations in
--      concepts (possibly with some way to specify what must be implemented
--      to avoid mutual infinite recursion like in Haskell typeclasses)

import Lang.Parser
import qualified Lang.Tokens as Tok
import Lang.Operator
import Text.Parsec.Pos(SourcePos, sourceLine, sourceColumn)

data Output = StdOut | OutFile FilePath
              deriving (Show, Read, Eq)

data SExpr a = Symbol String |
               Atom a |
               List [SExpr a]
               deriving (Show, Read, Eq)

class Lispable a where
    lispify :: a -> SExpr String

showSexp :: Show a => SExpr a -> String
showSexp (Symbol a) = a
showSexp (Atom a) = show a
showSexp (List xs) = "(" ++ unwords (map showSexp xs) ++ ")"

printSexp :: Show a => SExpr a -> IO ()
printSexp = putStrLn . showSexp

lispify' :: Lispable x => (String, x) -> SExpr String
lispify' (str, tpe) = List [Atom str, lispify tpe]

instance Lispable FileData where
    -- ((package name) &rest decl)
    lispify (FileData name decl) =
        List $ List [Symbol "package", Atom name] : map lispify decl

instance Lispable Decl where
    -- (include pos name (&rest hiding))
    -- (import pos name (&rest hiding))
    -- (module pos name &body rest)
    -- (function pos &rest body)
    -- (type pos name (&rest args) synonym)
    --   ; where vars is a list of (name type)
    -- (concept pos name (&rest args) ctx &rest vars)
    --   ; where vars is a list of (name type)
    -- (generic pos name type)
    -- (instance pos name (&rest args) ctx &rest vars)
    -- (class pos name (&rest args) parent (&optional (&rest children)) abstr &body body)
    -- (meta pos &rest body)
    -- (meta-expr pos meta)
    lispify (Include pos name hiding) =
        List $ [Symbol "include", lispify pos, Atom name, List $ map Atom hiding]
    lispify (Import pos name hiding) =
        List $ [Symbol "import", lispify pos, Atom name, List $ map Atom hiding]
    lispify (Module pos name internals) =
        List $ [Symbol "module", lispify pos, Atom name] ++ map lispify internals
    lispify (Function pos func) =
        case lispify func of
          List xs -> List $ [Symbol "function", lispify pos] ++ xs
          _ -> List [Symbol "function"]
    lispify (TypeDecl pos name args synonym) =
        List $ [Symbol "type", lispify pos, Atom name, List $ map Atom args,
                lispify synonym]
    lispify (Concept pos name args ctx vars) =
        List $ [Symbol "concept", lispify pos, Atom name,
                List $ map Atom args, lispify ctx] ++
             map lispify' vars
    lispify (Generic pos name type_) =
        List $ [Symbol "generic", lispify pos, Atom name, lispify type_]
    lispify (Instance pos name args ctx vars) =
        List $ [Symbol "instance", lispify pos, Atom name,
                List $ map lispify args, lispify ctx]
                 ++ map lispify vars
    lispify (Class pos name args parent children abstr inner) =
        List $ [Symbol "class", lispify pos, Atom name, List $ map Atom args,
                lispify parent, children', lispify abstr] ++ map lispify inner
            where children' = case children of
                                Nothing -> List []
                                Just xs -> List [List $ map lispify xs]
    lispify (Meta pos func) =
        case lispify func of
          List xs -> List $ [Symbol "meta", lispify pos] ++ xs
          _ -> List [Symbol "meta"]
    lispify (MetaDeclare pos decl) =
        List $ [Symbol "meta-expr", lispify pos, lispify decl]

instance Lispable FunctionDecl where
    -- (name (&optional type) &rest cases)
    --   ; where cases is a list of ((&rest pattern) stmt)
    lispify (FunctionDecl type_ name insides) =
        List $ [Atom name, optionalType] ++ map translateInside insides
        where optionalType = case type_ of
                               Just x -> List [lispify x]
                               Nothing -> List []
              translateInside (ptn, stmt) = List [List $ map lispify ptn, lispify stmt]

instance Lispable ClassDecl where
    -- (field pos name type)
    -- (method pos &rest body)
    -- (meta-expr pos expr)
    lispify (Field pos name type_) =
        List $ [Symbol "field", lispify pos, Atom name, lispify type_]
    lispify (Method pos func) =
        case lispify func of
          List xs -> List $ [Symbol "method", lispify pos] ++ xs
          _ -> List [Symbol "method"]
    lispify (MetaDeclClass pos decl) =
        List $ [Symbol "meta-expr", lispify pos, lispify decl]

instance Lispable Type where
    -- (with-context type context)
    lispify (Type type_ context) = List [lispify type_, lispify context]

instance Lispable Context where
    -- (context &rest types)
    lispify (Context ctx) = List $ Symbol "context" : map lispify ctx

instance Lispable TypeExpr where
    -- (op pos expr)
    -- (tuple-type pos acc &rest types)
    -- (named-type pos name acc &rest args)
    -- (func-type pos (&rest args) result)
    lispify (TypeOper pos oe) =
        List $ [Symbol "op", lispify pos, lispify oe]
    lispify (Tuple pos xs acc) =
        List $ [Symbol "tuple-type", lispify pos, lispify acc] ++ map lispify xs
    lispify (Named pos name args acc) =
        List $ [Symbol "named-type", lispify pos, Atom name, lispify acc] ++
             map lispify args
    lispify (Func pos args result) =
        List $ [Symbol "func-type", lispify pos, List $ map lispify args, lispify result]

instance Lispable Expr where
    -- (call pos expr &rest args)
    -- (dot pos lhs name &rest args
    -- (block pos &body rest)
    -- (lit pos literal)
    -- (tuple pos &rest args)
    -- (list pos &rest args)
    -- (declare pos decl)
    -- (asn pos pattern expr)
    -- (subscript pos expr &rest args)
    -- (id pos name)
    -- (op pos opexpr)
    -- (if pos cond true &optional false)
    -- (unless pos cond true &optional false)
    -- (for pos type expr body) ; where type is = or <-
    -- (case pos expr &rest clauses) ; where clauses are (pattern (&optional guard) body)
    -- (cond pos (&rest rest) &optional else) ; where rest is (expr0 expr1)
    -- (let pos (&rest vars) (&rest funcs) expr)
    --   ; where vars are (ptn expr), funcs are (name (&optional type) &rest impl), and
    --   ;       impl are ((&rest ptn) stmt)
    -- (lambda pos (&rest args) expr)
    -- (meta-expr pos expr)
    lispify (FunctionCall pos expr args) =
        List $ [Symbol "call", lispify pos, lispify expr] ++ map lispify args
    lispify (DotCall pos expr string args) =
        List $ [Symbol "dot", lispify pos, lispify expr, Atom string] ++ map lispify args
    lispify (Block pos stmts) =
        List $ [Symbol "block", lispify pos] ++ map lispify stmts
    lispify (Literal pos token) =
        List $ [Symbol "lit", lispify pos, lispify token]
    lispify (TupleExpr pos args) =
        List $ [Symbol "tuple", lispify pos] ++ map lispify args
    lispify (ListExpr pos args) =
        List $ [Symbol "list", lispify pos] ++ map lispify args
    lispify (VarAsn pos name expr) =
        List $ [Symbol "asn", lispify pos, lispify name, lispify expr]
    lispify (Subscript pos expr args) =
        List $ [Symbol "subscript", lispify pos, lispify expr] ++ map lispify args
    lispify (Ident pos str) =
        List $ [Symbol "id", lispify pos, Atom str]
    lispify (Oper pos expr) =
        List $ [Symbol "op", lispify pos, lispify expr]
    lispify (IfStmt pos if_ cond true false) =
        List $ [lispify if_, lispify pos, lispify cond, lispify true] ++
             case false of
               Just x -> [lispify x]
               Nothing -> []
    lispify (ForStmt pos for ptn expr body) =
        List $ [Symbol "for", lispify pos, lispify for, lispify ptn,
                lispify expr, lispify body]
    lispify (Case pos expr clauses) =
        List $ [Symbol "case", lispify pos, lispify expr] ++ map handleClause clauses
            where handleClause (ptn, Nothing, body) =
                      List [lispify ptn, List [], lispify body]
                  handleClause (ptn, Just x, body) =
                      List [lispify ptn, List [lispify x], lispify body]
    lispify (Cond pos rest else_) =
        List $ [Symbol "cond", lispify pos, clauses] ++ elseClause
            where clauses = List $ map (\(e0, e1) -> List [lispify e0, lispify e1]) rest
                  elseClause = case else_ of
                                 Just x -> [lispify x]
                                 Nothing -> []
    lispify (LetStmt pos vars funcs expr) =
        List $ [Symbol "let", lispify pos, List $ map singleExpr vars,
                List $ map funcExpr funcs, lispify expr]
             where singleExpr (var, val) = List [lispify var, lispify val]
                   funcExpr (name, type_, impl) =
                       List $ [Atom name, maybeType type_] ++ map doImpl impl
                   maybeType (Just x) = List [lispify x]
                   maybeType Nothing = List []
                   doImpl (ptns, expr) = List [List $ map lispify ptns, lispify expr]
    lispify (Lambda pos args expr) =
        List $ [Symbol "lambda", lispify pos, List $ map Atom args, lispify expr]
    lispify (MetaExpr pos decl) =
        List $ [Symbol "meta-expr", lispify pos, lispify decl]

instance Lispable MetaCall where
    -- (meta name &rest args)
    lispify (MetaCall str args) = List $ [Symbol "meta", Atom str] ++ map lispify args

instance Lispable Literal where
    -- (literal type &rest contents)
    lispify tok = List $ [Symbol "literal", Symbol type_] ++ contents
        where (type_, contents) =
                  case tok of
                    LReMatch w -> ("rematch", [Atom w])
                    LReSub w0 w1 -> ("resub", [Atom w0, Atom w1])
                    LNumber w0 w1 w2 -> ("number", [Symbol $ show w0, Atom w1,
                                                    Symbol $ show w2])
                    LCharacter ch -> ("character", [Atom [ch]])
                    LString toks -> ("string", map lispify toks)
                    LSymbol sym -> ("symbol", [Atom sym])

instance Lispable Tok.Token where
    -- (literal type &rest contents)
    lispify tok = List $ [Symbol "literal", Symbol type_] ++ contents
        where (type_, contents) =
                  case tok of
                    Tok.Keyword w -> ("keyword", [Atom w])
                    Tok.ReMatch w -> ("rematch", [Atom w])
                    Tok.ReSub w0 w1 -> ("resub", [Atom w0, Atom w1])
                    Tok.Identifier w -> ("id", [Atom w])
                    Tok.Number w0 w1 w2 -> ("number", [Symbol $ show w0, Atom w1,
                                                   Symbol $ show w2])
                    Tok.Character ch -> ("character", [Atom [ch]])
                    Tok.String toks -> ("string", map lispify toks)
                    Tok.Symbol sym -> ("symbol", [Atom sym])
                    Tok.Operator w -> ("operator", [Atom w])

instance Lispable Tok.TextToken where
    -- (text str)
    -- (interp str)
    lispify (Tok.Text str) = List [Symbol "text", Atom str]
    lispify (Tok.Interp str) = List [Symbol "interp", Atom str]

instance (Lispable o, Lispable a) => Lispable (OpExpr o a) where
    -- (operator &rest exprs)
    lispify (OpExpr a) = lispify a
    lispify (Pre op a) = List [lispify op, lispify a]
    lispify (Post a op) = List [lispify op, lispify a]
    lispify (Inf a1 op a2) = List [lispify op, lispify a1, lispify a2]

instance Lispable IfOp where
    lispify If = Symbol "if"
    lispify Unless = Symbol "unless"

instance Lispable ForOp where
    lispify ForEq = Symbol "="
    lispify ForBind = Symbol "<-"

instance Lispable Access where
    lispify Read = Symbol "read"
    lispify ReadWrite = Symbol "read-write"

instance Lispable Op where
    lispify Plus = Symbol "+"
    lispify Minus = Symbol "-"
    lispify Times = Symbol "*"
    lispify Div = Symbol "/"
    lispify BooleanAnd = Symbol "&&"
    lispify BooleanOr = Symbol "||"
    lispify Apply = Symbol "=~"
    lispify UniversalAnd = Symbol "and"
    lispify UniversalOr = Symbol "or"
    lispify Equal = Symbol "=="
    lispify NEqual = Symbol "!="
    lispify Less = Symbol "<"
    lispify Greater = Symbol ">"
    lispify LE = Symbol "<="
    lispify GE = Symbol ">="

instance Lispable TypeOp where
    lispify Union = Symbol "union"
    lispify Inter = Symbol "intersect"

instance Lispable Conditional where
    -- expr
    -- (bind lhs rhs)
    lispify (CondExpr a) = lispify a
    lispify (BindExpr lhs rhs) = List [Symbol "bind", lispify lhs, lispify rhs]

instance Lispable Pattern where
    -- (pattern-tuple pos &rest args)
    -- (pattern-list pos &rest args)
    -- (pattern-splat pos (&rest args0) splat (&rest args1))
    -- (pattern-id pos name)
    -- (pattern-type pos name &rest args)
    -- (pattern-expr pos expr)
    -- (pattern-underscore pos)
    -- (pattern-meta pos expr)
    lispify (TuplePattern pos args) =
        List $ [Symbol "pattern-tuple", lispify pos] ++ map lispify args
    lispify (ListPattern pos args) =
        List $ [Symbol "pattern-list", lispify pos] ++ map lispify args
    lispify (SplatPattern pos a0 mid a1) =
        List $ [Symbol "pattern-splat", lispify pos] ++
             map lispify a0 ++ [Atom mid] ++ map lispify a1
    lispify (IdPattern pos name) =
        List $ [Symbol "pattern-id", lispify pos, Atom name]
    lispify (TypePattern pos name args) =
        List $ [Symbol "pattern-type", lispify pos, Atom name] ++
             map lispify args
    lispify (ExprPattern pos expr) =
        List $ [Symbol "pattern-expr", lispify pos, lispify expr]
    lispify (UnderscorePattern pos) =
        List $ [Symbol "pattern-underscore", lispify pos]
    lispify (MetaPattern pos expr) =
        List $ [Symbol "pattern-meta", lispify pos, lispify expr]

instance Lispable Bool where
    -- true
    -- false
    lispify True = Symbol "true"
    lispify False = Symbol "false"

instance Lispable a => Lispable [a] where
    lispify = List . map lispify

instance Lispable SourcePos where
    lispify src = List [Symbol . show $ sourceLine src, Symbol . show $ sourceColumn src]

maybeToOutput :: Maybe FilePath -> Output
maybeToOutput (Just x) = OutFile x
maybeToOutput Nothing = StdOut

output :: Lispable a => Output -> a -> IO ()
output StdOut = printSexp . lispify
output (OutFile str) = writeFile str . showSexp . lispify
