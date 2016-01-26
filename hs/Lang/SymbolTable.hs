module Lang.SymbolTable(Environment(..), SymbolInterface(..),
                        PublicTable(..), PrivateTable(..),
                        SymbolTable(..), Validated, Unvalidated, ValueId(..),
                        MetaId(..), Instance(..), lookupValue, lookupMeta,
                        lookupPublicValue, lookupPublicMeta,
                        addValue, addMeta, addPublicValue, addPublicMeta,
                        updateValue, updatePublicValue, updatePackageValue,
                        resolveReference) where

import Lang.Identifier
import Lang.Error
import Lang.Parser
import Data.Monoid
import Data.Foldable
import Data.Maybe(catMaybes)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Text.Parsec.Pos

newtype Environment v = Environment (Map PackageName (SymbolInterface v))
    deriving (Show, Eq)

data SymbolInterface v = SymbolInterface {getSPrivateTable :: PrivateTable v,
                                          getSPublicTable :: PublicTable v}
                         deriving (Show, Eq)

newtype PrivateTable v = PrivateTable {getPrivateTable :: [(PackageName, [String])]}
    deriving (Show, Eq)

newtype PublicTable v = PublicTable {getPublicTable :: SymbolTable v}
    deriving (Show, Eq)

data SymbolTable v = SymbolTable {getValues :: Map RawName (ValueId v),
                                  getMetas :: Map RawName (MetaId v)}
                   deriving (Show, Eq)

-- Uninhabited types meant to be used as a phantom type to determine whether
-- the arguments to a class, type, or other construct have been validated.
-- This does NOT indicate that the body has been validated; that happens in
-- compilation. This is merely for the names in the arguments.
data Validated
data Unvalidated

data ValueId v = FunctionId SourcePos FunctionDecl |
                 TypeSynonym SourcePos RawName [DSName] TypeExpr |
                 ClassId SourcePos String [String] TypeExpr (Maybe [TypeExpr]) |
                 ConceptId SourcePos RawName [DSName] Context
                               [(RawName, Type)] [Instance] |
                 GenericId SourcePos RawName Type [(SourcePos, FunctionDecl)]
                 deriving (Show, Eq)

data MetaId v = MetaId SourcePos FunctionDecl
                deriving (Show, Eq)

data Instance = InstanceId SourcePos [TypeExpr] Context [FunctionDecl]
                deriving (Show, Eq)

instance Monoid (SymbolTable v) where
    mempty = SymbolTable mempty mempty
    SymbolTable a0 b0 `mappend` SymbolTable a1 b1 = SymbolTable (a0 <> a1) (b0 <> b1)

lookupValue :: RawName -> SymbolTable v -> Maybe (ValueId v)
lookupValue str (SymbolTable vv _) = Map.lookup str vv

lookupMeta :: RawName -> SymbolTable v -> Maybe (MetaId v)
lookupMeta str (SymbolTable _ mm) = Map.lookup str mm

lookupPublicValue :: RawName -> SymbolInterface v -> Maybe (ValueId v)
lookupPublicValue str (SymbolInterface _ (PublicTable pu)) = lookupValue str pu

lookupPublicMeta :: RawName -> SymbolInterface v -> Maybe (MetaId v)
lookupPublicMeta str (SymbolInterface _ (PublicTable pu)) = lookupMeta str pu

addValue :: RawName -> ValueId v -> SymbolTable v -> Maybe (SymbolTable v)
addValue str val (SymbolTable vv mm) =
    guard (Map.notMember str vv) >> return (SymbolTable (Map.insert str val vv) mm)

addMeta :: RawName -> MetaId v -> SymbolTable v -> Maybe (SymbolTable v)
addMeta str val (SymbolTable vv mm) =
    guard (Map.notMember str mm) >> return (SymbolTable vv (Map.insert str val mm))

addPublicMeta :: RawName -> MetaId v -> SymbolInterface v -> Maybe (SymbolInterface v)
addPublicMeta str val (SymbolInterface pr (PublicTable pu)) =
    SymbolInterface pr . PublicTable <$> addMeta str val pu

addPublicValue :: RawName -> ValueId v -> SymbolInterface v -> Maybe (SymbolInterface v)
addPublicValue str val (SymbolInterface pr (PublicTable pu)) =
    SymbolInterface pr . PublicTable <$> addValue str val pu

resolveReference :: RefName -> (Environment v, SymbolInterface v) ->
                    Either ResolutionError (PackageName, ValueId v)
resolveReference (Raw str) (Environment env, sym) =
    let local = (,) <$> pure (PackageName []) <*> lookupPublicValue str sym
        foreigns = map (\(x, y) -> (,) x <$> lookupPublicValue str y) $ Map.toList env
    in case catMaybes $ local : foreigns of
         [] -> Left $ NoSuchName (getRawName str)
         [answer] -> Right answer
         xs -> Left $ Ambiguous (getRawName str) (map fst xs)

updateValue :: RawName -> ValueId v -> SymbolTable v -> SymbolTable v
updateValue str val (SymbolTable vv mm) = SymbolTable (Map.insert str val vv) mm

updatePublicValue :: RawName -> ValueId v -> SymbolInterface v -> SymbolInterface v
updatePublicValue str val (SymbolInterface pr (PublicTable pu)) =
    SymbolInterface pr . PublicTable $ updateValue str val pu

updatePackageValue :: RawName -> ValueId v -> PackageName -> Environment v -> Environment v
updatePackageValue str val pkg (Environment env) =
    Environment $ Map.adjust (updatePublicValue str val) pkg env
