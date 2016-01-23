module Lang.Reader(parseFile) where

import Control.Monad.Trans.Except
import Control.Arrow
import Lang.Lexer(scan)
import Lang.Parser

liftError :: (Monad m, Show e) => Either e a -> ExceptT String m a
liftError = ExceptT . return . left show

parseFile :: FilePath -> ExceptT String IO FileData
parseFile file = do
  code <- ExceptT $ Right <$> readFile file
  lex <- liftError $ scan file code
  exp <- liftError $ parseCode file lex
  return exp
