module Lang.Validator(ValidateState, Validatable, validateEnv) where

import Lang.SymbolTable
import Lang.Error
import qualified Data.Map as Map
import Data.Traversable
import Control.Monad.Trans.Except
import Control.Monad.RWS

-- Validate State
-- Reader: Environment (excluding current package) and current package
-- Writer: Warnings
-- State: N/A
-- Except: Possible errors while processing
type ValidateState =
    RWST (Environment Unvalidated, SymbolInterface Unvalidated) [Warning] ()
             (Except LangError)

class Validatable f where
    validate :: f Unvalidated -> ValidateState (f Validated)

validateEnv :: Environment Unvalidated ->
               Except LangError (Environment Validated, [Warning])
validateEnv (Environment env) = do
  env' <- forM env $ \sym ->
              runRWST (validate sym)
                          (Environment $ Map.delete (getPackage sym) env, sym) ()
  let (w, a) = mapAccumR (\w0 (a, _, w1) -> (w0 ++ w1, a)) [] env'
  return (Environment a, w)

-- ///// Validatable instances for everything
instance Validatable SymbolInterface where
    validate = undefined
{-
    validate (SymbolInterface pkg insts gens pr pu) =
        SymbolInterface <$> pure pkg <*> traverse validate insts <*>
        traverse validate gens <*> validate pr <*> validate pu
-}
