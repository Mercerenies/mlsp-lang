module Lang.Error(LangError(..), ErrorType(..), Warning(..),
                  liftError, liftParseError, liftMaybe,
                  stdError, stdErrorPos) where

import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad.Trans.Except
import Control.Arrow

data LangError = ParserError ParseError |
                 StdError ErrorType (Maybe SourcePos) String
                 deriving (Eq)

data ErrorType = NameError | PackageError | MiscError
                 deriving (Show, Read, Eq, Ord, Enum)

data Warning = CircularWarning String
               deriving (Show, Read, Eq)

instance Show LangError where
    show (ParserError x) = show x
    show (StdError tp (Just pos) str) = show tp ++ " " ++ show pos ++ "\n" ++ str
    show (StdError tp Nothing str) = show tp ++ "\n" ++ str

liftError :: (Monad m, Show e) => Either e a -> ExceptT String m a
liftError = ExceptT . return . left show

liftParseError :: Monad m => Either ParseError a -> ExceptT LangError m a
liftParseError = ExceptT . return . left ParserError

liftMaybe :: (Monad m) => e -> Maybe a -> ExceptT e m a
liftMaybe str Nothing = throwE str
liftMaybe _ (Just x) = return x

stdError :: ErrorType -> String -> LangError
stdError err str = StdError err Nothing str

stdErrorPos :: ErrorType -> SourcePos -> String -> LangError
stdErrorPos err pos str = StdError err (Just pos) str
