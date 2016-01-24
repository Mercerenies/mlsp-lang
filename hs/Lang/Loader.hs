module Lang.Loader(Environment(..), SymbolTable(..), NameState,
                   loadNames) where

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

instance Monoid Environment where
    mempty = Environment mempty
    (Environment a) `mappend` (Environment b) = Environment $ a `mappend` b

-- Reader: Top-level main directory name
-- Writer: Warnings
-- State: Imported packages
-- Except: Possible errors while processing
type NameState = RWST String [Warning] [PackageName] (ExceptT LangError IO)

{- /////
loadMain :: String -> FileData
            -> IO (Either LangError (Environment, [PackageName], [Warning]))
loadMain str dat = runExceptT . runRWST (dropFileName str) [] $ loadNames str dat
-}

loadNames :: String -> FileData -> NameState Environment
loadNames pkg (FileData pkg1 decls) = do
  unless (pkg == pkg1) $
         lift . throwE . stdError PackageError $
                  "Expected package " ++ pkg ++ " got " ++ pkg1 ++ "."
  pkg' <- lift . liftMaybe (stdError NameError "Non-package name encountered") $
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
  dir <- ask
  let pkg' = dir </> foldr (</>) "" pkg
  code <- lift $ parseFile pkg'
  loadNames pkg' code
resolveImport _ = return mempty
