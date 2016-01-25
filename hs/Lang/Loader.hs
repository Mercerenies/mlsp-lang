module Lang.Loader(ReadState(..), NameState,
                   loadMain, loadNames) where

import Lang.Parser
import Lang.Reader
import Lang.Identifier
import Lang.Error
import Lang.SymbolTable
import Data.Maybe(catMaybes)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Trans.Except
import System.FilePath

data ReadState = ReadState {getToplevelDirectory :: FilePath,
                            getCurrentFilename :: FilePath}

instance Monoid (Environment v) where
    mempty = Environment mempty
    (Environment a) `mappend` (Environment b) = Environment $ a `mappend` b

-- Name State
-- Reader: Read state
-- Writer: Warnings
-- State: Imported packages
-- Except: Possible errors while processing
type NameState = RWST ReadState [Warning] [PackageName] (ExceptT LangError IO)

-- Public Resolution State
-- Reader: N/A
-- Writer: Warnings
-- State: Environment (excluding current package) / Current package symbol interface
-- Except: Possible errors while processing
type PublicResState v =
    RWST () [Warning] (Environment v, SymbolInterface v) (ExceptT LangError IO)

loadMain :: String -> FileData
            -> IO (Either LangError (Environment Unvalidated, [PackageName], [Warning]))
loadMain str dat = runExceptT $ runRWST (loadNames (fromPackageName mainPackageName) dat)
                   (ReadState (dropFileName str) str) []

loadNames :: String -> FileData -> NameState (Environment Unvalidated)
loadNames pkg (FileData pkgDecl decls) = do
  ReadState {getCurrentFilename = fname} <- ask
  -- Validate the package name
  unless (pkg == pkgDecl) $
         lift . throwE . stdErrorFile PackageError fname $
                  "Expected package " ++ pkg ++ " got " ++ pkgDecl ++ "."
  pkg' <- lift . liftMaybe (stdErrorFile NameError fname "Non-package name encountered") $
                                      toPackageName pkg
  pkgs <- get
  -- Check for circular imports
  if pkg' `elem` pkgs then do
                        lift . throwE .
                           stdErrorFile PackageError fname $ "Circular import on " ++ pkg
  else do
    -- Insert into the package list to avoid future circular imports
    modify (pkg' :)
    env0 <- mconcat <$> mapM resolveImport decls
    private <- resolvePrivateNames decls
    let sym0 = SymbolInterface private (PublicTable mempty)
    (env1, sym1) <- resolvePublicNames (env0, sym0) decls
    let Environment env1' = env1
        env2 = Environment $ Map.insert pkg' sym1 env1'
    modify tail
    return env2

resolveImport :: Decl -> NameState (Environment Unvalidated)
resolveImport (Import pos name _) = do -- The hiding clause is resolved later
  pkg <- lift . liftMaybe (stdErrorPos NameError pos "Non-package name encountered") $
             toPackageName name
  ReadState {getToplevelDirectory = dir} <- ask
  let pkg' = dir </> foldr (</>) "" (getPackageName pkg)
  code <- lift $ parseFile pkg'
  local (\x -> x {getCurrentFilename = pkg'}) $ loadNames (fromPackageName pkg) code
resolveImport _ = return mempty

resolvePublicNames :: (Environment Unvalidated, SymbolInterface Unvalidated) -> [Decl]
                   -> NameState (Environment Unvalidated, SymbolInterface Unvalidated)
resolvePublicNames es decls = do
  let stack0 = mapM_ resolvePublicName decls
  ((), es', warns) <- lift $ runRWST stack0 () es
  tell warns
  return es'

-- Just quietly ignore any invalid imports. They should have already been handled
-- by resolveImport
resolvePrivateNames :: [Decl] -> NameState (PrivateTable Unvalidated)
resolvePrivateNames decls =
    return . PrivateTable . catMaybes $ map resolvePrivateName decls

resolvePrivateName :: Decl -> Maybe (PackageName, [String])
resolvePrivateName (Import _ name hiding) = do
  pkg <- toPackageName name
  return (pkg, hiding)
resolvePrivateName _ = Nothing

-- TODO Maybe add an 'import X hiding (*)' syntax for just qualified imports

-- TODO Implement modules and includes here
-- TODO Generic functions and their implementations
resolvePublicName :: Decl -> PublicResState Unvalidated ()
resolvePublicName (Import {}) =
    return mempty
resolvePublicName (Include pos _ _) =
    lift . throwE $ stdErrorPos NotYetImplemented pos "Include statement"
resolvePublicName (Module pos _ _) =
    lift . throwE $ stdErrorPos NotYetImplemented pos "Module statement"
resolvePublicName (Function _ _) =
    undefined -- TODO This (functions and gen impls)
resolvePublicName (TypeDecl pos name args expr) = do
  (env, SymbolInterface pr (PublicTable sym)) <- get
  name' <- lift . liftMaybe (invalidNameError pos name) $ toRawName name
  args' <- mapM (\x -> lift . liftMaybe (invalidNameError pos x) $ toDSName x) args
  let synonym = TypeSynonym pos name' args' expr
  sym' <- case addValue name' synonym sym of
            Nothing -> lift . throwE $ nameConflictError pos name
            Just x -> return x
  put (env, SymbolInterface pr (PublicTable sym'))
resolvePublicName _ = error "Not implemented!" -- /////
-- TODO [()] is isomorphic to Nat??? Think about this!!!
-- TODO SourcePos is printing bizarrely; change Show for it if possible
