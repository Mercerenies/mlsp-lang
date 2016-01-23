module Lang.Loader where

import Lang.Parser
import Lang.Identifier
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

newtype Environment = Environment (Map PackageName SymbolTable)

data SymbolTable = SymbolTable
                   deriving (Show, Read, Eq)

loadNames :: FileData -> State Environment SymbolTable
loadNames _ = undefined
