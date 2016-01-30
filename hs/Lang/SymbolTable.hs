module Lang.SymbolTable(Environment(..), SymbolInterface(..),
                        PublicTable(..), PrivateTable(..), FunctionDecl'(..),
                        SymbolTable(..), Validated, Unvalidated, ValueId(..),
                        MetaId(..), ClassInner(..), Instance(..), InnerName,
                        GenMethod(..),
                        lookupValue, lookupMeta, lookupPublicValue, lookupPublicMeta,
                        addValue, addMeta, addPublicValue, addPublicMeta,
                        updateValue, updatePublicValue, updatePackageValue,
                        resolveReference, handleFunctionDecl,
                        classInnerName, getInnerName, addToClass,
                        addInstance, addGenMethod) where

import Lang.Identifier
import Lang.Error
import Lang.Parser
import Data.Monoid
import Data.Maybe(catMaybes)
import Data.Typeable
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Text.Parsec.Pos

newtype Environment v = Environment (Map PackageName (SymbolInterface v))
    deriving (Show, Eq)

data SymbolInterface v = SymbolInterface {getPackage :: PackageName,
                                          getInstances :: [Instance v],
                                          getGenMethods :: [GenMethod v],
                                          getSPrivateTable :: PrivateTable v,
                                          getSPublicTable :: PublicTable v}
                         deriving (Show, Eq)

newtype PrivateTable v = PrivateTable {getPrivateTable :: [(PackageName, [RawName])]}
    deriving (Show, Eq)

newtype PublicTable v = PublicTable {getPublicTable :: SymbolTable v}
    deriving (Show, Eq)

type InnerName = Either AtName RawName

data SymbolTable v = SymbolTable {getValues :: Map RawName (ValueId v),
                                  getMetas :: Map RawName (MetaId v)}
                   deriving (Show, Eq)

-- Uninhabited types meant to be used as a phantom type to determine whether
-- the arguments to a class, type, or other construct have been validated.
-- This does NOT indicate that the body has been validated; that happens in
-- compilation. This is merely for the names in the arguments.
data Validated deriving (Typeable)
data Unvalidated deriving (Typeable)

data FunctionDecl' v = FunctionDecl' (Maybe Type) RawName FunctionBody
                       deriving (Show, Eq)

data ValueId v = FunctionId SourcePos (FunctionDecl' v) |
                 TypeSynonym SourcePos RawName [DSName] TypeExpr |
                 ClassId SourcePos RawName [DSName] (Maybe TypeExpr) (Maybe [TypeExpr])
                     Bool (Map InnerName (ClassInner v)) |
                 ConceptId SourcePos RawName [DSName] Context
                               [(RawName, Type)] |
                 GenericId SourcePos RawName Type |
                 ConceptFuncId SourcePos RawName RawName -- Func Name, Conc Name
                 deriving (Show, Eq)

data MetaId v = MetaId SourcePos FunctionDecl
                deriving (Show, Eq)

data ClassInner v = FieldId SourcePos AtName TypeExpr |
                    MethodId SourcePos (FunctionDecl' v)
                    deriving (Show, Eq)

data Instance v = InstanceId SourcePos RawName [TypeExpr] Context [FunctionDecl' v]
                  deriving (Show, Eq)

data GenMethod v = GenMethod SourcePos (FunctionDecl' v)
                   deriving (Show, Eq)

instance Monoid (SymbolTable v) where
    mempty = SymbolTable mempty mempty
    SymbolTable a0 b0 `mappend` SymbolTable a1 b1 = SymbolTable (a0 <> a1) (b0 <> b1)

lookupValue :: RawName -> SymbolTable v -> Maybe (ValueId v)
lookupValue str (SymbolTable vv _) = Map.lookup str vv

lookupMeta :: RawName -> SymbolTable v -> Maybe (MetaId v)
lookupMeta str (SymbolTable _ mm) = Map.lookup str mm

lookupPublicValue :: RawName -> SymbolInterface v -> Maybe (ValueId v)
lookupPublicValue str (SymbolInterface {getSPublicTable = PublicTable pu}) =
    lookupValue str pu

lookupPublicMeta :: RawName -> SymbolInterface v -> Maybe (MetaId v)
lookupPublicMeta str (SymbolInterface {getSPublicTable = PublicTable pu}) =
    lookupMeta str pu

lookupPrivateValue :: Environment v -> RawName -> SymbolInterface v ->
                      Either ResolutionError (PackageName, ValueId v)
lookupPrivateValue (Environment env) str
                   (SymbolInterface {getSPrivateTable = PrivateTable pv}) =
    let RawName str' = str
        pv' = catMaybes . map (\(x, _) -> (,) x <$> Map.lookup x env) .
              filter (\(_, xs) -> not $ str `elem` xs) $ pv
    in case catMaybes $ map (\(y, x) -> (,) y <$> lookupPublicValue str x) pv' of
         [] -> Left $ NoSuchName str'
         [yx] -> Right yx
         xs -> Left . Ambiguous str' $ map fst xs

addValue :: RawName -> ValueId v -> SymbolTable v -> Maybe (SymbolTable v)
addValue str val (SymbolTable vv mm) =
    guard (Map.notMember str vv) >> return (SymbolTable (Map.insert str val vv) mm)

addMeta :: RawName -> MetaId v -> SymbolTable v -> Maybe (SymbolTable v)
addMeta str val (SymbolTable vv mm) =
    guard (Map.notMember str mm) >> return (SymbolTable vv (Map.insert str val mm))

addPublicMeta :: RawName -> MetaId v -> SymbolInterface v -> Maybe (SymbolInterface v)
addPublicMeta str val sym@(SymbolInterface {getSPublicTable = PublicTable pu}) =
    (\x -> sym {getSPublicTable = x}) . PublicTable <$> addMeta str val pu

addPublicValue :: RawName -> ValueId v -> SymbolInterface v -> Maybe (SymbolInterface v)
addPublicValue str val sym@(SymbolInterface {getSPublicTable = PublicTable pu}) =
    (\x -> sym {getSPublicTable = x}) . PublicTable <$> addValue str val pu

resolveReference :: PackageName -> RefName -> (Environment v, SymbolInterface v) ->
                    Either ResolutionError (PackageName, ValueId v)
resolveReference _ (Raw str) (Environment env, sym) =
    let local = (,) <$> pure (PackageName []) <*> lookupPublicValue str sym
        foreigns = lookupPrivateValue (Environment env) str sym
    in case (local, foreigns) of
         (Nothing, Right x) -> Right x
         (Nothing, Left err) -> Left err
         (Just x, Left (NoSuchName {})) -> Right x
         (Just _, Left (Ambiguous name pkgs)) ->
             Left $ Ambiguous name (PackageName [] : pkgs)
         (Just _, Right (pkg, _)) ->
             Left $ Ambiguous (getRawName str) [pkg, PackageName []]
resolveReference curPkg (Qualified pkg str) (Environment env, sym)
    | pkg == curPkg =
        case lookupPublicValue str sym of
          Nothing -> Left . NoSuchName $ getRawName str
          Just x -> Right (PackageName [], x)
    | not $ pkg `elem` (map fst . getPrivateTable $ getSPrivateTable sym) =
        Left . NoSuchName $ fromPackageName pkg
    | otherwise =
        case Map.lookup pkg env of
          Nothing -> Left . NoSuchName $ fromPackageName pkg
          Just sym1 -> case lookupPublicValue str sym1 of
                         Nothing -> Left . NoSuchName $ getRawName str
                         Just x -> Right (pkg, x)

updateValue :: RawName -> ValueId v -> SymbolTable v -> SymbolTable v
updateValue str val (SymbolTable vv mm) = SymbolTable (Map.insert str val vv) mm

updatePublicValue :: RawName -> ValueId v -> SymbolInterface v -> SymbolInterface v
updatePublicValue str val sym@(SymbolInterface {getSPublicTable = PublicTable pu}) =
    (\x -> sym {getSPublicTable = x}) . PublicTable $ updateValue str val pu

updatePackageValue :: RawName -> ValueId v -> PackageName -> Environment v -> Environment v
updatePackageValue str val pkg (Environment env) =
    Environment $ Map.adjust (updatePublicValue str val) pkg env

handleFunctionDecl :: FunctionDecl -> Maybe (FunctionDecl' Unvalidated)
handleFunctionDecl (FunctionDecl type_ name body) = do
  name' <- toRawName name
  return $ FunctionDecl' type_ name' body

classInnerName :: ClassInner v -> InnerName
classInnerName (FieldId _ name _) = Left name
classInnerName (MethodId _ (FunctionDecl' _ name _)) = Right name

getInnerName :: InnerName -> String
getInnerName (Left (AtName name)) = name
getInnerName (Right (RawName name)) = name

addToClass :: ClassInner v -> Map InnerName (ClassInner v) ->
              Maybe (Map InnerName (ClassInner v))
addToClass inner cls = let name = classInnerName inner
                       in case Map.lookup name cls of
                            Nothing -> Just $ Map.insert name inner cls
                            Just _ -> Nothing

addInstance :: Instance v -> SymbolInterface v -> SymbolInterface v
addInstance inst sym = sym {getInstances = inst : getInstances sym}

addGenMethod :: GenMethod v -> SymbolInterface v -> SymbolInterface v
addGenMethod inst sym = sym {getGenMethods = inst : getGenMethods sym}
