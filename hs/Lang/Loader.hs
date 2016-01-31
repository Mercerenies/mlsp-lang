module Lang.Loader(ReadState(..), NameState, PublicResState,
                   loadMain, loadNames) where

import Text.Parsec.Pos
import Lang.Parser
import Lang.Reader
import Lang.Identifier
import Lang.Error
import Lang.SymbolTable
import Data.Maybe(catMaybes)
-- import Data.Map(Map)
import qualified Data.Map as Map(empty)
import Control.Monad
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Control.Arrow
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
    let sym0 = SymbolInterface pkg' [] [] private (PublicTable mempty)
    (env1, sym1) <- resolvePublicNames (env0, sym0) decls
    let env2 = addPackage sym1 env1
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

resolvePrivateName :: Decl -> Maybe (PackageName, [RawName])
resolvePrivateName (Import _ name hiding) = do
  pkg <- toPackageName name
  hiding' <- mapM toRawName hiding
  return (pkg, hiding')
resolvePrivateName _ = Nothing

-- TODO Maybe add an 'import X hiding (*)' syntax for just qualified imports

throwMaybe :: LangError -> Maybe a -> PublicResState v a
throwMaybe err Nothing  = lift $ throwE err
throwMaybe _   (Just x) = return x

toIdName :: SourcePos -> String -> PublicResState v RawName
toIdName pos x = throwMaybe (invalidNameError pos x) $ toRawName x

toArgName :: SourcePos -> String -> PublicResState v DSName
toArgName pos x = throwMaybe (invalidNameError pos x) $ toDSName x

toFieldName :: SourcePos -> String -> PublicResState v AtName
toFieldName pos x = throwMaybe (invalidNameError pos x) $ toAtName x

handleFunc :: SourcePos -> FunctionDecl ->
              PublicResState Unvalidated (FunctionDecl' Unvalidated)
handleFunc pos func@(FunctionDecl _ n _) =
    throwMaybe (invalidNameError pos n) $ handleFunctionDecl func

-- TODO Implement modules and includes here
-- TODO Comment this behemoth immensely
resolvePublicName :: Decl -> PublicResState Unvalidated ()
resolvePublicName (Import {}) =
    return mempty
resolvePublicName (Include pos _ _) =
    lift . throwE $ stdErrorPos NotYetImplemented pos "Include statement"
resolvePublicName (Module pos _ _) =
    lift . throwE $ stdErrorPos NotYetImplemented pos "Module statement"
resolvePublicName (Function pos decl) = do
  -- Either merge into generic or create a function
  (env, sym) <- get
  let FunctionDecl _ name _ = decl
  name' <- case toRefName name of
             Nothing -> lift . throwE $ invalidNameError pos name
             Just x -> return x
  case resolveReference (getPackage sym) name' (env, sym) of
    Right (_, GenericId {}) ->
        do
          decl' <- handleFunc pos decl
          let sym' = addGenMethod (GenMethod pos decl') sym
          put (env, sym')
    Right (PackageName (_:_), _) ->
        case name' of
          Raw name'' -> newFunction (env, sym) (pos, decl) name''
          Qualified {} -> lift . throwE $ invalidNameError pos name
    Right (_, _) -> lift . throwE $ nameConflictError pos name
    Left _ -> case name' of
                Raw name'' -> newFunction (env, sym) (pos, decl) name''
                Qualified {} -> lift . throwE $ invalidNameError pos name
 where newFunction :: (Environment Unvalidated, SymbolInterface Unvalidated) ->
                      (SourcePos, FunctionDecl) ->
                      RawName ->
                      PublicResState Unvalidated ()
       newFunction (env, sym) (pos, decl) name' = do
           decl' <- handleFunc pos decl
           let func = FunctionId pos decl'
           case addPublicValue name' func sym of
             Nothing -> lift . throwE $ nameConflictError pos (getRawName name')
             Just sym' -> put (env, sym')
resolvePublicName (TypeDecl pos name args expr) = do
  -- Add the type to the current package
  -- Current package cannot have matching name but imports can
  (env, sym) <- get
  name' <- toIdName pos name
  args' <- mapM (toArgName pos) args
  let synonym = TypeSynonym pos name' args' expr
  sym' <- throwMaybe (nameConflictError pos name) $ addPublicValue name' synonym sym
  put (env, sym')
resolvePublicName (Class pos name args parent children abstr decls) = do
  (env, sym) <- get
  let declError pos = nameConflictError pos . getInnerName . classInnerName
  name' <- toIdName pos name
  args' <- mapM (toArgName pos) args
  decls' <- concat <$> mapM resolveClassName decls
  decls'' <- foldM (\x y -> throwMaybe (declError pos y) $ addToClass y x)
             Map.empty decls'
  let cls = ClassId pos name' args' parent children abstr decls''
  sym' <- throwMaybe (nameConflictError pos name) $ addPublicValue name' cls sym
  put (env, sym')
resolvePublicName (Concept pos name args ctx inner) = do
  -- Add the concept to the current package
  -- Current package cannot have matching name but imports can
  (env, sym) <- get
  name' <- toIdName pos name
  args' <- mapM (toArgName pos) args
  inner' <- (mapM $ \(x, y) -> (,) <$> toIdName pos x <*> pure y) inner
  let concept = ConceptId pos name' args' ctx inner'
  sym' <- throwMaybe (nameConflictError pos name) $ addPublicValue name' concept sym
  put (env, sym')
  forM_ inner' $ \(fname, _) -> do
    (env0, sym0) <- get
    let cfi = ConceptFuncId pos fname name'
        RawName fname' = fname
    sym1 <- throwMaybe (nameConflictError pos fname') $ addPublicValue fname cfi sym0
    put (env0, sym1)
resolvePublicName (Instance pos name args ctx decls) = do
  -- Merge the instance into its concept
  (env, sym) <- get
  name' <- case toRawName name of
             Nothing -> lift . throwE $ invalidNameError pos name
             Just x -> return x
  (_, ref) <- lift . ExceptT . return . left (resolutionError pos) $
              resolveReference (getPackage sym) (Raw name') (env, sym)
  decls' <- mapM (handleFunc pos) decls
  let inst = InstanceId pos name' args ctx decls'
  sym' <- case ref of
             ConceptId {} ->
                 return $ addInstance inst sym
             _ -> lift $ throwE (stdErrorPos ReferenceError pos $
                                             show name ++ " is not a concept")
  put (env, sym')
resolvePublicName (Generic pos name type_) = do
  (env, sym) <- get
  name' <- toIdName pos name
  let gen = GenericId pos name' type_
  sym' <- throwMaybe (nameConflictError pos name) $ addPublicValue name' gen sym
  put (env, sym')
resolvePublicName (Meta pos decl) = do
  (env, sym) <- get
  let FunctionDecl _ name _ = decl
      meta = MetaId pos decl
  name' <- toIdName pos name
  sym' <- throwMaybe (nameConflictError pos name) $ addPublicMeta name' meta sym
  put (env, sym')
resolvePublicName (MetaDeclare pos _) =
    lift . throwE $ stdErrorPos NotYetImplemented pos "Meta calls"

resolveClassName :: ClassDecl ->
                    PublicResState Unvalidated [ClassInner Unvalidated]
resolveClassName (Field pos name type_) = do
  name' <- toFieldName pos name
  return . pure $ FieldId pos name' type_
resolveClassName (Method pos decl) = do
  pure . MethodId pos <$> handleFunc pos decl
resolveClassName (MetaDeclClass pos _) = do
  lift . throwE $ stdErrorPos NotYetImplemented pos "Meta calls"

-- TODO [()] is isomorphic to Nat??? Think about this!!!
