module Interp (
  interpProg
  ) where

import Control.Monad.State
import Ast
import Symtab (Symtab, Id, empty, add, fold, keys)
import qualified Symtab (get)
import Eval (evalAExp, runEvalAExp)

-- Interpret a program by evaluating its AExp
interpProg :: Prog -> Integer
interpProg p =
  -- Create initial state with function bindings.
  -- We are sure to use foldl here, so that [keys acc] always refers to
  -- all of the keys added to the ctx thus far. Since the functions are
  -- listed in the order in which they appear in the program, the
  -- function scoping rules are implemented by this.
  let s = foldl (\acc f -> add (fid_of f) (Right (f, keys acc)) acc)
          empty (funs_of p) in
    runEvalAExp s (evalAExp (eval_of p))

