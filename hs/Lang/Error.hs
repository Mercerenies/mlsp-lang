module Lang.Error where

import Text.Parsec.Error
import Control.Monad.Trans.Except
import Control.Arrow

data LangError = ParserError ParseError |
                 NameError String
                 deriving (Eq)

instance Show LangError where
    show (ParserError x) = show x
    show (NameError x) = x

liftError :: (Monad m, Show e) => Either e a -> ExceptT String m a
liftError = ExceptT . return . left show

liftParseError :: Monad m => Either ParseError a -> ExceptT LangError m a
liftParseError = ExceptT . return . left ParserError
