module Lang.Validator(ValidateState, Validatable,
                      validateEnv, validateTypeExpr) where

import Lang.SymbolTable
import Lang.Error
import Lang.Parser
import Lang.Operator
import Lang.Identifier
import qualified Data.Map as Map
import Data.Traversable
import Text.Parsec.Pos(SourcePos, newPos)
import Control.Monad.Trans.Except
import Control.Monad.RWS
import Control.Arrow

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

resolveWithError :: SourcePos -> RefName -> (Environment v, SymbolInterface v) ->
                    ValidateState (PackageName, ValueId v)
resolveWithError pos name =
    lift . except . left (resolutionError pos) . resolveReference name

validateTypeExpr :: TypeExpr -> ValidateState ()
validateTypeExpr (TypeOper _ expr) =
    let validateOp (OpExpr x) = validateTypeExpr x
        validateOp (Pre _ x) = validateOp x
        validateOp (Post x _) = validateOp x
        validateOp (Inf x _ y) = validateOp x >> validateOp y
    in void $ validateOp expr
validateTypeExpr (Tuple _ exprs) = mapM_ validateTypeExpr exprs
validateTypeExpr (Func _ args res) = mapM_ validateTypeExpr args <* validateTypeExpr res
validateTypeExpr (Named pos name exprs) = do
  envsym <- ask
  name' <- lift . liftMaybe (invalidNameError pos name) $ toRefName name
  (_, val) <- resolveWithError pos name' envsym
  argn <- case val of
            TypeSynonym _ _ xs _ -> return $ length xs
            ClassId _ _ xs _ _ _ _ -> return $ length xs
            _ -> lift . throwE $ stdErrorPos ReferenceError pos $
                   name ++ " is not a valid type"
  mapM_ validateTypeExpr exprs
  unless (length exprs == argn) $
         (lift . throwE $ stdErrorPos ArgumentError pos
               ("Expected " ++ show argn ++ " args to " ++ show name ++ ", got " ++
                            (show $ length exprs)))
  return ()

instance Validatable SymbolInterface where
    validate (SymbolInterface pkg insts gens pr pu) =
        SymbolInterface <$> pure pkg <*> traverse validate insts <*>
        traverse validate gens <*> validate pr <*> validate pu

instance Validatable PrivateTable where
    validate (PrivateTable pr) = do
      traverse checkName (concatMap (\(x0, xs) -> map (\x1 -> (x0, x1)) xs) pr)
      return $ PrivateTable pr
        where checkName (pkg, raw) = do
                                    envsym <- ask
                                    let ref = Qualified pkg raw
                                    -- TODO Actual position here, not newPos
                                    resolveWithError (newPos "" 0 0) ref envsym
                                    return ()

instance Validatable PublicTable where
    validate (PublicTable v) = PublicTable <$> validate v

instance Validatable Instance where
    validate = undefined

instance Validatable GenMethod where
    validate (GenMethod pos decl) = GenMethod pos <$> validate decl

instance Validatable SymbolTable where
    validate = undefined

instance Validatable FunctionDecl' where
    validate = undefined
