module Lang.Loader(Environment(..), SymbolTable(..), ReadState(..), NameState,
                   loadMain, loadNames) where

import Lang.Parser
import Lang.Reader
import Lang.Identifier
import Lang.Error
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Trans.Except
import System.FilePath

newtype Environment = Environment (Map PackageName SymbolTable)
    deriving (Show, Read, Eq)

data SymbolTable = SymbolTable
                   deriving (Show, Read, Eq)

data ReadState = ReadState {getToplevelDirectory :: String,
                            getCurrentFilename :: String}

instance Monoid Environment where
    mempty = Environment mempty
    (Environment a) `mappend` (Environment b) = Environment $ a `mappend` b

-- Reader: Read state
-- Writer: Warnings
-- State: Imported packages
-- Except: Possible errors while processing
type NameState = RWST ReadState [Warning] [PackageName] (ExceptT LangError IO)

loadMain :: String -> FileData
            -> IO (Either LangError (Environment, [PackageName], [Warning]))
loadMain str dat = runExceptT $ runRWST (loadNames (fromPackageName mainPackageName) dat)
                   (ReadState (dropFileName str) str) []

loadNames :: String -> FileData -> NameState Environment
loadNames pkg (FileData pkg1 decls) = do
  ReadState {getCurrentFilename = fname} <- ask
  unless (pkg == pkg1) $
         lift . throwE . stdErrorFile PackageError fname $
                  "Expected package " ++ pkg ++ " got " ++ pkg1 ++ "."
  pkg' <- lift . liftMaybe (stdErrorFile NameError fname "Non-package name encountered") $
                                      toPackageName pkg
  pkgs <- get
  if pkg' `elem` pkgs then do
                        tell [CircularWarning $ "Circular import on " ++ pkg]
                        return mempty
  else do
    modify (pkg' :)
    mconcat <$> mapM resolveImport decls

resolveImport :: Decl -> NameState Environment
resolveImport (Import pos name _) = do -- The hiding clause is resolved later
  pkg <- lift . liftMaybe (stdErrorPos NameError pos "Non-package name encountered") $
             toPackageName name
  ReadState {getToplevelDirectory = dir} <- ask
  let pkg' = dir </> foldr (</>) "" (getPackageName pkg)
  code <- lift $ parseFile pkg'
  local (\x -> x {getCurrentFilename = pkg'}) $ loadNames (fromPackageName pkg) code
resolveImport _ = return mempty
