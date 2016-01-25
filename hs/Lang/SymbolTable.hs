module Lang.SymbolTable(Environment(..), SymbolInterface(..),
                        PublicTable(..), PrivateTable(..),
                        SymbolTable(..), Validated, Unvalidated, ValueId(..),
                        MetaId(..), Instance, lookupValue, lookupMeta,
                        addValue, addMeta) where

import Lang.Identifier
import Lang.Parser
import Data.Monoid
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
                 ConceptId SourcePos String [String] Context [(String, Type)] [Instance] |
                 GenericId SourcePos String Type [(SourcePos, FunctionDecl)]
                 deriving (Show, Eq)

data MetaId v = MetaId SourcePos FunctionDecl
                deriving (Show, Eq)

data Instance = InstanceId SourcePos [TypeExpr] Context [Decl]
                deriving (Show, Eq)

instance Monoid (SymbolTable v) where
    mempty = SymbolTable mempty mempty
    SymbolTable a0 b0 `mappend` SymbolTable a1 b1 = SymbolTable (a0 <> a1) (b0 <> b1)

lookupValue :: RawName -> SymbolTable v -> Maybe (ValueId v)
lookupValue str (SymbolTable vv _) = Map.lookup str vv

lookupMeta :: RawName -> SymbolTable v -> Maybe (MetaId v)
lookupMeta str (SymbolTable _ mm) = Map.lookup str mm

addValue :: RawName -> ValueId v -> SymbolTable v -> Maybe (SymbolTable v)
addValue str val (SymbolTable vv mm) =
    guard (Map.notMember str vv) >> return (SymbolTable (Map.insert str val vv) mm)

addMeta :: RawName -> MetaId v -> SymbolTable v -> Maybe (SymbolTable v)
addMeta str val (SymbolTable vv mm) =
    guard (Map.notMember str mm) >> return (SymbolTable vv (Map.insert str val mm))
