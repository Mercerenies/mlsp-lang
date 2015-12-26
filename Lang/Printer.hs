module Lang.Printer where

import Data.List(intercalate)
import Data.Map(toList)
import Lang.Parser
import qualified Lang.Tokens as Tok
import Lang.Operator

data Output = StdOut | OutFile String
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
    lispify = undefined

instance Lispable a => Lispable (OpExpr a) where
    lispify = undefined
