module EvalZ3 (
  checkSat, evalCompileM
  ) where

import           Control.Monad.State

import           Z3.Monad (AST, Z3)
import qualified Z3.Monad as Z3

import Ast
import Symtab (Symtab, Id(..), empty, get, add, keys)
import Compile (CompileM, Context, compileBExp, compileProg, Z3Var)

-- Run the solver on a formula. If sat, the stringified model is returned.
checkSat :: CompileM () -> CompileM String
checkSat exec = do
  exec
  result <- Z3.solverCheckAndGetModel
  case result of
    (Z3.Sat, Just model) -> Z3.modelToString model
    (res, _)             -> return $ show res

-- Given an initial state (unroll bound and a presumably empty list of
-- assertions) and a monadic action for running the solver, produce
-- the final string result in the IO monad.
evalCompileM :: (Integer, [AST]) -> CompileM String -> IO String
evalCompileM init_state eval =
  Z3.evalZ3 $ evalStateT eval init_state
