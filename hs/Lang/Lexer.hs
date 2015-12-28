module Lang.Lexer (scan, tokens) where

import Data.Char(isSpace)
import Lang.Tokens hiding (satisfy, identifier, keyword, operator)
import Text.Parsec hiding (tokens)
import Control.Applicative hiding (many, (<|>))
import Control.Monad

alNumUnder :: Parsec String () Char
alNumUnder = alphaNum <|> char '_'

alphaUnder :: Parsec String () Char
alphaUnder = letter <|> char '_'

scan :: String -> String -> Either ParseError [Lexeme]
scan src str = parse tokens src (str ++ ";")
-- TODO Consider a less cheat-y alternative to this semicolon here

tokens :: Parsec String () [Lexeme]
tokens = contents <* many (void spaceish <|> comment) <* eof
    where contents = many $ try spacedToken
          spacedToken = do
            many (void spaceish <|> comment)
            pos <- getPosition
            newline_ <|> flip Token pos <$> token_
          spaceish = satisfy $ liftA2 (&&) isSpace (not . (`elem` "\r\n"))
          comment = try blockComment <|> try lineComment

token_ :: Parsec String () Token
token_ = try keyword <|>
         try reMatch <|>
         try reSub <|>
         try identifier <|>
         try number <|>
         try character <|>
         try string_ <|>
         try symbol <|>
         try operator

newline_ :: Parsec String () Lexeme
newline_ = Newline <$ oneOf "\r\n;"

keyword :: Parsec String () Token
keyword = Keyword <$> choice (map (try . string) keywords)

operator :: Parsec String () Token
operator = Operator <$> choice (map (try . string) operators)

identifier :: Parsec String () Token
identifier = do
  str <- (:) <$> (char '$' <|> char '@' <|> alphaUnder) <*> many alNumUnder
  return $ Identifier str

integerNumber :: Parsec String () Integer
integerNumber = do
  sign <- option '+' (oneOf "+-")
  digits <- read <$> many1 digit
  case sign of
    '+' -> return $   digits
    '-' -> return $ - digits
    _   -> unexpected $ "sign " ++ show sign

number :: Parsec String () Token
number = do
  integerPart <- integerNumber
  decimal <- option "" $ char '.' *> many1 digit
  exp <- option 0 $ oneOf "Ee" *> integerNumber
  return $ Number integerPart decimal exp

character :: Parsec String () Token
character = do
  string "#/"
  next <- Left <$> many1 letter <|>
          Right <$> anyChar
  case next of
    Right literal -> return $ Character literal
    Left "space" -> return $ Character ' '
    Left "tab" -> return $ Character '\t'
    Left "newline" -> return $ Character '\n'
    Left "return" -> return $ Character '\r'
    Left [ch] -> return $ Character ch
    Left name -> unexpected $ "character name " ++ show name

string_ :: Parsec String () Token
string_ = rawString <|> interString

symbol :: Parsec String () Token
symbol = Symbol <$> ((++) <$> (char ':' *> option "" (string "$")) <*>
                          many1 (alphaNum <|> char '_'))

rawString :: Parsec String () Token
rawString = do
  text <- char '\'' *> many (satisfy (/= '\'')) <* char '\''
  return . String $ [Text text]

reDelim :: Parsec String () Char
reDelim = oneOf reDelimiters

reMatch :: Parsec String () Token
reMatch = do
  char 'm'
  delim <- reDelim
  contents <- many $ satisfy (/= delim)
  char delim
  return $ ReMatch contents

reSub :: Parsec String () Token
reSub = do
  char 's'
  delim <- reDelim
  contents1 <- many $ satisfy (/= delim)
  char delim
  contents2 <- many $ satisfy (/= delim)
  char delim
  return $ ReSub contents1 contents2

interString :: Parsec String () Token
interString = do
  char '"'
  contents <- many $ do
                next <- satisfy (/= '"')
                case next of
                  '\\' -> do
                       after <- anyChar
                       return $ case after of
                                  't' -> Text "\t"
                                  'r' -> Text "\r"
                                  'n' -> Text "\n"
                                  _   -> Text [after]
                  '#' -> do
                       after <- lookAhead anyChar
                       case after of
                         '{' -> do
                            char '{'
                            interp <- identifier >>= \i ->
                                      case i of
                                        Identifier str -> return str
                                        _ -> unexpected "interpolated object"
                            char '}'
                            return $ Interp interp
                         _   -> return $ Text "#"
                  _ -> return $ Text [next]
  char '"'
  let compress curr [] = [curr]
      compress curr (Interp x : xs) = curr : Interp x : xs
      compress curr (Text x : xs) = case curr of
                                      Text y -> Text (y ++ x) : xs
                                      Interp _ -> curr : Text x : xs
  return . String . foldr compress [] $ contents

lineComment :: Parsec String () ()
lineComment = void $ string "##" <* many (noneOf "\r\n") <* oneOf "\r\n"

blockComment :: Parsec String () ()
blockComment = void $ string "#|" <* many blockCommentInternals <* string "|#"

blockCommentInternals :: Parsec String () ()
blockCommentInternals = do
  notFollowedBy $ string "|#"
  void (try blockComment) <|> void anyChar
