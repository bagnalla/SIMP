-- This module defines the interpreter evaluation functions.

module Eval (
  evalAExp, runEvalAExp, showPos
  ) where

import           Control.Monad.Identity
import           Control.Monad.State

import           Ast
import           Lexer (AlexPosn(..))
import           Symtab (Id(..), Symtab)
import           Symtab (add, empty)
import qualified Symtab (get)

-- A FunEntry is a function definition along with the Ids of other
-- functions that are allowed to be called from within the function.
type FunEntry = (Fun, [Id])

-- State maps Ids to either integer values or function entries
type EvalState = Symtab (Either Integer FunEntry)

-- The evaluation state monad
type EvalM = StateT EvalState Identity

-------------------
-- Helper functions

showPos p =
  case p of
    AlexPn char line col -> show line ++ ":" ++ show col

lookupInt :: AlexPosn -> Id -> EvalState -> Integer
lookupInt pos id s =
  case Symtab.get id s of
    Just (Left n)  -> n
    Just (Right _) -> error $ "expected integer variable at " ++ showPos pos
    Nothing        -> error $ "unbound variable at " ++ showPos pos

lookupFunEntry :: AlexPosn -> Id -> EvalState -> FunEntry
lookupFunEntry pos id s =
  case Symtab.get id s of
    Just (Left _)  -> error $ "expected function id at " ++ showPos pos
    Just (Right e) -> e
    Nothing        -> error $ "unbound variable at " ++ showPos pos

lookupFun :: AlexPosn -> Id -> EvalState -> Fun
lookupFun pos id s = let (f, _) = lookupFunEntry pos id s in f

lookupFunScope :: AlexPosn -> Id -> EvalState -> [Id]
lookupFunScope pos id s = let (_, scope) = lookupFunEntry pos id s in scope

-------------------------------------------------
-- Big-step evaluation of arithmetic expressions.

evalAExp :: AExp -> EvalM Integer
evalAExp (ANum n) = return n
evalAExp (AVar pos id) = do
  s <- get
  return $ lookupInt pos id s
evalAExp (ACall pos fid args) = do
  s <- get
  arg_vals <- mapM evalAExp args
  let (f, f_scope) = lookupFunEntry pos fid s
  let funEntries = map (\fid -> lookupFunEntry pos fid s) f_scope
  let ctx = if elem (out_of f) (params_of f)
            then empty
            else (add (out_of f) (Left 0) empty)
  let ctx' = foldr (\(param, arg) acc -> add param (Left arg) acc)
        ctx (zip (params_of f) arg_vals)
  let ctx'' = foldr (\(fid, funEntry) acc -> add fid (Right funEntry) acc) ctx'
        (zip f_scope funEntries)
  put ctx''
  evalCom (body_of f)
  s' <- get
  let n = lookupInt pos (out_of f) s'
  put s
  return n
evalAExp (APlus a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 + n2
evalAExp (AMinus a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 - n2
evalAExp (AMult a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 * n2
evalAExp (ADiv a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ div n1 n2

----------------------------------------------
-- Big-step evaluation of boolean expressions.

evalBExp :: BExp -> EvalM Bool
evalBExp BTrue = return True
evalBExp BFalse = return False
evalBExp (BLt a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 < n2
evalBExp (BLe a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 <= n2
evalBExp (BEq a1 a2) = do
  n1 <- evalAExp a1
  n2 <- evalAExp a2
  return $ n1 == n2
evalBExp (BOr b1 b2) = do
  v1 <- evalBExp b1
  v2 <- evalBExp b2
  return $ v1 || v2
evalBExp (BAnd b1 b2) = do
  v1 <- evalBExp b1
  v2 <- evalBExp b2
  return $ v1 && v2
evalBExp (BNot b) =
  return . not =<< evalBExp b

-----------------------------------
-- Big-step evaluation of commands.

evalCom :: Com -> EvalM ()
evalCom CSkip = return ()
evalCom (CAss id a) = do
  n <- evalAExp a
  s <- get
  put $ add id (Left n) s
evalCom (CSeq c1 c2) = evalCom c1 >> evalCom c2
evalCom (CIf b c1 c2) = do
  b' <- evalBExp b
  if b'
    then evalCom c1
    else evalCom c2
evalCom c@(CWhile b c1) = do
  b' <- evalBExp b
  if b'
    then evalCom c1 >> evalCom c
    else return ()

--------------------
-- Run the evaluator

runEvalAExp :: EvalState -> EvalM Integer -> Integer
runEvalAExp init_state eval =
  fst $ runIdentity (runStateT eval init_state)
