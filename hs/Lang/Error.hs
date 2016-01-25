module Lang.Error(LangError(..), ErrorType(..), Warning(..),
                  liftError, liftParseError, liftMaybe,
                  stdError, stdErrorPos, stdErrorFile,
                  invalidNameError, nameConflictError) where

import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad.Trans.Except
import Control.Arrow

data LangError = ParserError ParseError |
                 StdError ErrorType ErrorPos String
                 deriving (Eq)

data ErrorType = NameError | PackageError | NotYetImplemented | MiscError
                 deriving (Show, Read, Eq, Ord, Enum)

data ErrorPos = NoPos | Pos SourcePos | FilePos FilePath
                deriving (Show, Eq)

data Warning = CircularWarning String
               deriving (Show, Read, Eq)

instance Show LangError where
    show (ParserError x) = show x
    show (StdError tp (Pos pos) str) = show tp ++ " " ++ show pos ++ "\n" ++ str
    show (StdError tp (FilePos pos) str) = show tp ++ " " ++ pos ++ "\n" ++ str
    show (StdError tp NoPos str) = show tp ++ "\n" ++ str

liftError :: (Monad m, Show e) => Either e a -> ExceptT String m a
liftError = ExceptT . return . left show

liftParseError :: Monad m => Either ParseError a -> ExceptT LangError m a
liftParseError = ExceptT . return . left ParserError

liftMaybe :: (Monad m) => e -> Maybe a -> ExceptT e m a
liftMaybe str Nothing = throwE str
liftMaybe _ (Just x) = return x

stdError :: ErrorType -> String -> LangError
stdError err str = StdError err NoPos str

stdErrorPos :: ErrorType -> SourcePos -> String -> LangError
stdErrorPos err pos str = StdError err (Pos pos) str

stdErrorFile :: ErrorType -> FilePath -> String -> LangError
stdErrorFile err pos str = StdError err (FilePos pos) str

invalidNameError :: SourcePos -> String -> LangError
invalidNameError pos str =
    stdErrorPos NameError pos $ "Invalid identifier " ++ str

nameConflictError :: SourcePos -> String -> LangError
nameConflictError pos str =
    stdErrorPos NameError pos $ "Name " ++ str ++ " already defined"
