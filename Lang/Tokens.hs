{-# LANGUAGE FlexibleContexts #-}

module Lang.Tokens(Newline(..), Token(..), TextToken(..), Lexeme, EParser,
                   satisfy, matchLexeme, matchToken, newline, newlines,
                   newlines1, identifier, keyword, operator,
                   keywords, operators, reDelimiters) where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Pos(updatePosChar)

data Newline = Newline
               deriving (Show, Read, Eq)

data Token =
    Keyword String |
    ReMatch String |
    ReSub String String |
    Identifier String |
    WIdentifier String | -- Identifier with ! suffix
    Number Integer String Integer | -- Integer part, decimal part, exponent part
    Character Char |
    String [TextToken] |
    Symbol String |
    Operator String
    deriving (Show, Read, Eq)

data TextToken = Text String | Interp String
                 deriving (Show, Read, Eq)

type Lexeme = Either Token Newline

type EParser = Parsec [Lexeme] ()

-- TODO Keep track of source position (this doesn't work for columns and honestly
--      doesn't work too well for rows either right now)
satisfy :: Stream s m Lexeme => (Lexeme -> Bool) -> ParsecT s u m Lexeme
satisfy p = tokenPrim show update test
    where update src (Right Newline) _ = updatePosChar src '\n'
          update src (Left _) _ = updatePosChar src ' '
          test x = guard (p x) >> return x

matchLexeme :: Stream s m Lexeme => Lexeme -> ParsecT s u m Lexeme
matchLexeme lex = satisfy (== lex)

matchToken :: Stream s m Lexeme => Token -> ParsecT s u m Lexeme
matchToken = matchLexeme . Left

newline :: Stream s m Lexeme => ParsecT s u m Lexeme
newline = matchLexeme (Right Newline) <?> "newline"

newlines :: Stream s m Lexeme => ParsecT s u m ()
newlines = void . many $ matchLexeme (Right Newline)

newlines1 :: Stream s m Lexeme => ParsecT s u m ()
newlines1 = newline >> newlines

identifier :: Stream s m Lexeme => ParsecT s u m String
identifier = do
  let isIdentifier x = case x of
                         Left (Identifier _) -> True
                         _ -> False
  ident <- satisfy isIdentifier <?> "identifier"
  case ident of
    Left (Identifier str) -> return str
    _ -> unexpected "non-identifier token"

keyword :: Stream s m Lexeme => String -> ParsecT s u m Lexeme
keyword str = matchToken (Keyword str) <?> "keyword '" ++ str ++ "'"

operator :: Stream s m Lexeme => String -> ParsecT s u m Lexeme
operator op = matchToken (Operator op) <?> "operator '" ++ op ++ "'"

keywords :: [String]
keywords = ["module", "type", "begin", "end", "concept", "instance",
            "fields", "if", "then", "else", "unless", "in", "case",
            "when", "cond", "for", "var", "dynamic"]

operators :: [String]
operators = ["+=", "-=", "*=", "/=", "==", "<=", ">=", "&&=", "||=", "&&", "||",
             "...", "->", "<-", "=~", "::", "<", ">", "*", "/", "[", "]", "&",
             "{", "}", ",", "=", ".", "+", "-", "(", ")", "|", "^",
             "and", "or", "is"]

reDelimiters :: [Char]
reDelimiters = "/~!@#$%^&*|-=+"

-- For / case statements
