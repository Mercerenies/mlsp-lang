module Lang.Reader(parseFile, liftError) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Lang.Lexer(scan)
import Lang.Parser
import Lang.Error

parseFile :: FilePath -> ExceptT LangError IO FileData
parseFile file = do
  code <- lift $ readFile file
  lex <- liftParseError $ scan file code
  exp <- liftParseError $ parseCode file lex
  return exp
