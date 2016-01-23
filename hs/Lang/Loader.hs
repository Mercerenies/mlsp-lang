module Lang.Loader(Environment(..), SymbolTable(..), loadNames) where

import Lang.Parser
import Lang.Identifier
import Lang.Error
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

newtype Environment = Environment (Map PackageName SymbolTable)

data SymbolTable = SymbolTable
                   deriving (Show, Read, Eq)

loadNames :: FileData -> StateT Environment (Except LangError) SymbolTable
loadNames _ = undefined

