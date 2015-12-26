module Lang.Printer(Output(..), SExpr(..), Lispable(..),
                    showSexp, printSexp, output) where

import Data.List(intercalate)
import Data.Map(toList)
import Lang.Parser
import qualified Lang.Tokens as Tok
import Lang.Operator

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
showSexp (List xs) = "(" ++ intercalate " " (map showSexp xs) ++ ")"

printSexp :: Show a => SExpr a -> IO ()
printSexp = putStrLn . showSexp

lispify' :: Lispable x => (String, x) -> SExpr String
lispify' (str, tpe) = List [Atom str, lispify tpe]

instance Lispable Decl where
    -- (module name &body rest)
    -- (function name (&rest args) (&optional type) body)
    -- (type name parent (&rest vars) fields) ; where vars is a list of (name type)
    -- (concept name (&rest args) &rest vars) ; where vars is a list of (name type)
    -- (instance name (&rest args) impl &rest vars)
    -- (var name (&optional type) body)
    lispify (Module name internals) =
        List $ [Symbol "module", Atom name] ++ map lispify internals
    lispify (Function type_ name args impl) =
        List $ [Symbol "function", Atom name, List $ map Atom args,
                List (maybe [] (\x -> [lispify x]) type_), lispify impl]
    lispify (Type name parent vars fields) =
        List $ [Symbol "type", Atom name, lispify parent, List $ map lispify' vars,
                lispify fields]
    lispify (Concept name args vars) =
        List $ [Symbol "concept", Atom name, List $ map Atom args] ++ map lispify' vars
    lispify (Instance name args impl vars) =
        List $ [Symbol "instance", Atom name, List $ map lispify args, lispify impl]
                 ++ map lispify vars
    lispify (Variable type_ name val) =
        List $ [Symbol "var", Atom name, List (maybe [] (\x -> [lispify x]) type_),
                lispify val]

instance Lispable Type where
    -- (tuple-type &rest types)
    -- (named-type name &rest args)
    -- (func-type (&rest args) result)
    lispify (Tuple xs) = List $ Symbol "tuple-type" : map lispify xs
    lispify (Named name args) = List $ [Symbol "named-type", Atom name] ++ map lispify args
    lispify (Func args result) = List $ [Symbol "func-type", List $ map lispify args,
                                              lispify result]

instance Lispable Stmt where
    -- (stmt expr &optional cond)
    --   cond == (if expr) or (unless expr)
    lispify (Stmt expr cond) = List $ [Symbol "stmt", lispify expr] ++ handleCond cond
        where handleCond Nothing = []
              handleCond (Just (If, expr1)) = [Symbol "if", lispify expr1]
              handleCond (Just (Unless, expr1)) = [Symbol "unless", lispify expr1]

instance Lispable Fields where
    -- (&rest fields) ; where each field is (name access)
    lispify (Fields mp) = List . map convert $ toList mp
        where convert (str, acc) = List [Atom str,
                                         Symbol $ case acc of
                                                    Read -> "read"
                                                    ReadWrite -> "read-write"]

instance Lispable Expr where
    -- (call expr &rest args)
    -- (block &body rest)
    -- literal
    -- (tuple &rest args)
    -- (list &rest args)
    -- (declare decl)
    -- (asn name expr)
    -- (subscript expr &rest args)
    -- (id name)
    -- stmt
    -- opexpr
    lispify (FunctionCall expr args) =
        List $ [Symbol "call", lispify expr] ++ map lispify args
    lispify (DotCall expr string args) =
        List $ [Symbol "call", lispify $ Ident string, lispify expr] ++ map lispify args
    lispify (Block stmts) =
        List $ Symbol "block" : map lispify stmts
    lispify (Literal token) =
        lispify token
    lispify (TupleExpr args) =
        List $ Symbol "tuple" : map lispify args
    lispify (ListExpr args) =
        List $ Symbol "list" : map lispify args
    lispify (Declare decl) =
        List $ [Symbol "declare", lispify decl]
    lispify (VarAsn name expr) =
        List $ [Symbol "asn", Atom name, lispify expr]
    lispify (Subscript expr args) =
        List $ [Symbol "subscript", lispify expr] ++ map lispify args
    lispify (Ident str) =
        List [Symbol "id", Atom str]
    lispify (Statement stmt) =
        lispify stmt
    lispify (Oper expr) =
        lispify expr

instance Lispable Tok.Token where
    -- (literal type &rest contents)
    lispify tok = List $ [Symbol "literal", Symbol type_] ++ contents
        where (type_, contents) =
                  case tok of
                    Tok.Keyword w -> ("keyword", [Atom w])
                    Tok.ReMatch w -> ("rematch", [Atom w])
                    Tok.ReSub w0 w1 -> ("resub", [Atom w0, Atom w1])
                    Tok.Identifier w -> ("id", [Atom w])
                    Tok.WIdentifier w -> ("wid", [Atom w])
                    Tok.Number w0 w1 w2 -> ("number", [Symbol $ show w0, Symbol w1,
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

instance Lispable a => Lispable [a] where
    lispify = List . map lispify

output :: Lispable a => Output -> a -> IO ()
output StdOut = printSexp . lispify
output (OutFile str) = writeFile str . showSexp . lispify
