{-# LANGUAGE FlexibleContexts #-}

module Lang.Tokens(Token(..), TextToken(..), Lexeme(..), EParser,
                   satisfy, matchLexeme, matchToken, newline, newlines,
                   newlines1, identifier, keyword, operator,
                   keywords, operators, reDelimiters) where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Pos

data Token =
    Keyword String |
    ReMatch String |
    ReSub String String |
    Identifier String |
    Number Integer String Integer | -- Integer part, decimal part, exponent part
    Character Char |
    String [TextToken] |
    Symbol String |
    Operator String
    deriving (Show, Read, Eq)

data TextToken = Text String | Interp String
                 deriving (Show, Read, Eq)

data Lexeme = Token Token SourcePos |
              Newline
              deriving (Show, Eq)

type EParser = Parsec [Lexeme] ()

satisfy :: Stream s m Lexeme => (Lexeme -> Bool) -> ParsecT s u m Lexeme
satisfy p = tokenPrim show update test
    where update _   (Token _ pos) _ = pos
          update src Newline   _ = src
          test x = guard (p x) >> return x

matchLexeme :: Stream s m Lexeme => Lexeme -> ParsecT s u m Lexeme
matchLexeme lex = satisfy (== lex)

matchToken :: Stream s m Lexeme => Token -> ParsecT s u m Lexeme
matchToken tok = satisfy $ \x -> case x of
                                   Token y _ -> y == tok
                                   _ -> False

newline :: Stream s m Lexeme => ParsecT s u m Lexeme
newline = matchLexeme Newline <?> "newline"

newlines :: Stream s m Lexeme => ParsecT s u m ()
newlines = void . many $ matchLexeme Newline

newlines1 :: Stream s m Lexeme => ParsecT s u m ()
newlines1 = newline >> newlines

identifier :: Stream s m Lexeme => ParsecT s u m String
identifier = do
  let isIdentifier x = case x of
                         Token (Identifier _) _ -> True
                         _ -> False
  ident <- satisfy isIdentifier <?> "identifier"
  case ident of
    Token (Identifier str) _ -> return str
    _ -> unexpected "non-identifier token"

keyword :: Stream s m Lexeme => String -> ParsecT s u m Lexeme
keyword str = matchToken (Keyword str) <?> "keyword '" ++ str ++ "'"

operator :: Stream s m Lexeme => String -> ParsecT s u m Lexeme
operator op = matchToken (Operator op) <?> "operator '" ++ op ++ "'"

keywords :: [String]
keywords = ["package", "import", "include", "hiding", "module", "type", "begin", "end",
            "concept", "instance", "fields", "if", "then", "else", "unless",
            "in", "case", "when", "cond", "for", "var", "dynamic"]

operators :: [String]
operators = ["+=", "-=", "*=", "/=", "==", "<=", ">=", "&&=", "||=", "&&", "||",
             "...", "->", "<-", "=~", "::", "<", ">", "*", "/", "[", "]", "&", "!",
             "{", "}", ",", "=", ".", "+", "-", "(", ")", "|", "^",
             "and", "or", "is", "has"]

reDelimiters :: [Char]
reDelimiters = "/~!@#$%^&*|-=+"
