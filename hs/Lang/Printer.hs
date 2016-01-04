module Lang.Printer(Output(..), SExpr(..), Lispable(..),
                    showSexp, printSexp, output) where

-- TODO Alter the syntax slightly to allow default implementations in
--      concepts (possibly with some way to specify what must be implemented
--      to avoid mutual infinite recursion like in Haskell typeclasses)

import Data.Map(toList)
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
    -- (function pos name (&rest args) (&optional type) body)
    -- (type pos name (&rest args) parent (&rest vars) fields)
    --   ; where vars is a list of (name type)
    -- (concept pos name (&rest args) timing &rest vars)
    --   ; where vars is a list of (name type)
    -- (instance pos name (&rest args) impl &rest vars)
    lispify (Include pos name hiding) =
        List $ [Symbol "include", lispify pos, Atom name, List $ map Atom hiding]
    lispify (Import pos name hiding) =
        List $ [Symbol "import", lispify pos, Atom name, List $ map Atom hiding]
    lispify (Module pos name internals) =
        List $ [Symbol "module", lispify pos, Atom name] ++ map lispify internals
    lispify (Function pos type_ name args impl) =
        List $ [Symbol "function", lispify pos, Atom name, List $ map Atom args,
                List (maybe [] (\x -> [lispify x]) type_), lispify impl]
    lispify (Type pos name args parent vars fields) =
        List $ [Symbol "type", lispify pos, Atom name, List $ map Atom args,
                lispify parent, List $ map lispify' vars, lispify fields]
    lispify (Concept pos name args bind vars) =
        List $ [Symbol "concept", lispify pos, Atom name, List $ map Atom args, timing] ++
             map lispify' vars
        where timing = case bind of
                         Static -> Symbol "static"
                         Dynamic -> Symbol "dynamic"
    lispify (Instance pos name args impl vars) =
        List $ [Symbol "instance", lispify pos, Atom name,
                List $ map lispify args, lispify impl]
                 ++ map lispify vars

instance Lispable Type where
    -- (tuple-type pos acc &rest types)
    -- (named-type pos name acc &rest args)
    -- (func-type pos (&rest args) result)
    lispify (Tuple pos xs acc) =
        List $ [Symbol "tuple-type", lispify pos, lispify acc] ++ map lispify xs
    lispify (Named pos name args acc) =
        List $ [Symbol "named-type", lispify pos, Atom name, lispify acc] ++
             map lispify args
    lispify (Func pos args result) =
        List $ [Symbol "func-type", lispify pos, List $ map lispify args, lispify result]

instance Lispable Fields where
    -- (&rest fields) ; where each field is (name access)
    lispify (Fields mp) = List . map convert $ toList mp
        where convert (str, acc) = List [Atom str, lispify acc]

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
    -- (let pos (&rest rest) expr) ; where rest is (ptn expr)
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
    lispify (Declare pos decl) =
        List $ [Symbol "declare", lispify pos, lispify decl]
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
    lispify (LetStmt pos clauses expr) =
        List $ [Symbol "let", lispify pos, List $ map singleExpr clauses, lispify expr]
             where singleExpr (var, val) = List [lispify var, lispify val]

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

instance Lispable a => Lispable (OpExpr a) where
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

instance Lispable a => Lispable [a] where
    lispify = List . map lispify

instance Lispable SourcePos where
    lispify src = List [Symbol . show $ sourceLine src, Symbol . show $ sourceColumn src]

output :: Lispable a => Output -> a -> IO ()
output StdOut = printSexp . lispify
output (OutFile str) = writeFile str . showSexp . lispify
