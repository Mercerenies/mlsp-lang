module Lang.Loader(Environment(..), SymbolInterface(..), SymbolTable(..),
                   PrivateTable(..), PublicTable(..), ReadState(..), NameState,
                   Validated, Unvalidated, ValueId(..), MetaId(..),
                   loadMain, loadNames) where

import Text.Parsec.Pos
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

newtype Environment v = Environment (Map PackageName (SymbolInterface v))
    deriving (Show, Eq)

data SymbolInterface v = SymbolInterface {getSPrivateTable :: PrivateTable v,
                                          getSPublicTable :: PublicTable v}
                         deriving (Show, Eq)

newtype PrivateTable v = PrivateTable {getPrivateTable :: SymbolTable v}
    deriving (Show, Eq)

newtype PublicTable v = PublicTable {getPublicTable :: SymbolTable v}
    deriving (Show, Eq)

data SymbolTable v = SymbolTable {getValues :: Map String (ValueId v),
                                  getMetas :: Map String (MetaId v)}
                   deriving (Show, Eq)

-- Uninhabited types meant to be used as a phantom type to determine whether
-- the arguments to a class, type, or other construct have been validated.
-- This does NOT indicate that the body has been validated; that happens in
-- compilation. This is merely for the names in the arguments.
data Validated
data Unvalidated

data ValueId v = FunctionId SourcePos FunctionDecl |
                 TypeSynonym SourcePos String [String] TypeExpr |
                 ClassId SourcePos String [String] TypeExpr (Maybe [TypeExpr]) |
                 ConceptId SourcePos String [String] Context [(String, Type)] [Instance] |
                 GenericId SourcePos String Type [(SourcePos, FunctionDecl)]
                 deriving (Show, Eq)

data MetaId v = MetaId SourcePos FunctionDecl
                deriving (Show, Eq)

data Instance = InstanceId SourcePos [TypeExpr] Context [Decl]
                deriving (Show, Eq)

data ReadState = ReadState {getToplevelDirectory :: FilePath,
                            getCurrentFilename :: FilePath}

instance Monoid (Environment v) where
    mempty = Environment mempty
    (Environment a) `mappend` (Environment b) = Environment $ a `mappend` b

instance Monoid (SymbolTable v) where
    mempty = SymbolTable mempty mempty
    SymbolTable a0 b0 `mappend` SymbolTable a1 b1 = SymbolTable (a0 <> a1) (b0 <> b1)

-- Reader: Read state
-- Writer: Warnings
-- State: Imported packages
-- Except: Possible errors while processing
type NameState = RWST ReadState [Warning] [PackageName] (ExceptT LangError IO)

loadMain :: String -> FileData
            -> IO (Either LangError (Environment Unvalidated, [PackageName], [Warning]))
loadMain str dat = runExceptT $ runRWST (loadNames (fromPackageName mainPackageName) dat)
                   (ReadState (dropFileName str) str) []

loadNames :: String -> FileData -> NameState (Environment Unvalidated)
loadNames pkg (FileData pkgDecl decls) = do
  ReadState {getCurrentFilename = fname} <- ask
  unless (pkg == pkgDecl) $
         lift . throwE . stdErrorFile PackageError fname $
                  "Expected package " ++ pkg ++ " got " ++ pkgDecl ++ "."
  pkg' <- lift . liftMaybe (stdErrorFile NameError fname "Non-package name encountered") $
                                      toPackageName pkg
  pkgs <- get
  if pkg' `elem` pkgs then do
                        lift . throwE .
                           stdErrorFile PackageError fname $ "Circular import on " ++ pkg
  else do
    modify (pkg' :)
    env0 <- mconcat <$> mapM resolveImport decls
    public <- resolvePublicNames decls
    private <- resolvePrivateNames decls
    return $ env0 <> (Environment . Map.singleton pkg' $ SymbolInterface private public)

resolveImport :: Decl -> NameState (Environment Unvalidated)
resolveImport (Import pos name _) = do -- The hiding clause is resolved later
  pkg <- lift . liftMaybe (stdErrorPos NameError pos "Non-package name encountered") $
             toPackageName name
  ReadState {getToplevelDirectory = dir} <- ask
  let pkg' = dir </> foldr (</>) "" (getPackageName pkg)
  code <- lift $ parseFile pkg'
  local (\x -> x {getCurrentFilename = pkg'}) $ loadNames (fromPackageName pkg) code
resolveImport _ = return mempty

resolvePublicNames :: [Decl] -> NameState (PublicTable Unvalidated)
resolvePublicNames decls = PublicTable . mconcat <$> mapM resolvePublicName decls

resolvePrivateNames :: [Decl] -> NameState (PrivateTable Unvalidated)
resolvePrivateNames _ = return $ PrivateTable mempty

-- TODO Implement modules and includes here
-- TODO Generic functions and their implementations
resolvePublicName :: Decl -> NameState (SymbolTable Unvalidated)
resolvePublicName (Import {}) =
    return mempty -- Imports to not introduce public names
resolvePublicName (Include pos _ _) =
    lift . throwE $ stdErrorPos NotImplementedError pos
             "Include statements are not yet implemented"
resolvePublicName (Module pos _ _) =
    lift . throwE $ stdErrorPos NotImplementedError pos "Modules are not yet implemented"
resolvePublicName (Function pos decl@(FunctionDecl _ name _)) =
    return $ SymbolTable (Map.singleton name $ FunctionId pos decl) mempty
resolvePublicName (TypeDecl pos name args expr) =
    return $ SymbolTable (Map.singleton name $ TypeSynonym pos name args expr
resolvePublicName _ = undefined -- ///// Think about how generics are going to merge
                                -- (Probably going to have to write custom Monoid
                                --  instance)
