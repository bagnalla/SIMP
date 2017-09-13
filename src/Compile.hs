module Compile (
  CompileM,
  compileBExp,
  compileProg,
  Context,
  Z3Var
  ) where

import           Control.Monad.State
import           Control.Monad (foldM, foldM_)

import           Z3.Monad      (AST, Z3, MonadZ3, (+?))
import qualified Z3.Monad      as Z3
import           Data.Maybe    (fromJust)

import           Ast
import           Eval (showPos)
import           Lexer (AlexPosn(..))
import           Symtab(Symtab, Id, empty, add, keys)
import qualified Symtab(get)

type Z3Var = AST

-- A FunEntry is a function definition along with the Ids of other
-- functions that are allowed to be called from within the function.
type FunEntry = (Fun, [Id])

-- A context maps IMP Ids to either Z3Vars or FunEntrys.
type Context = Symtab (Either Z3Var FunEntry)

-- We extend the Z3 monad with our own state for the loop unrolling
-- bound. We leave the context out of the state because we have to do a
-- bunch of manual manipulation of contexts which is a bit awkward when
-- it's in the state. It wouldn't be terrible, but either way is fine.
-- EDIT: also include the list of assertions that we're building up so
-- that we can negate their conjunction at the end.
type CompileM = StateT (Integer, [AST]) Z3

-- MonadZ3 instance for StateT
instance MonadZ3 b => MonadZ3 (StateT a b) where
  getSolver = StateT $ \s -> do
    solver <- Z3.getSolver
    return (solver, s)
  getContext = StateT $ \s -> do
    ctx <- Z3.getContext
    return (ctx, s)

-------------------
-- Helper functions

dummypos = AlexPn 0 0 0

lookupZ3Var :: AlexPosn -> Id -> Context -> Z3Var
lookupZ3Var pos id s =
  case Symtab.get id s of
    Just (Left var)  -> var
    Just (Right _) -> error $ "expected integer variable at " ++ showPos pos
    Nothing        -> error $ "unbound variable at " ++ showPos pos

lookupFunEntry :: AlexPosn -> Id -> Context -> FunEntry
lookupFunEntry pos id s =
  case Symtab.get id s of
    Just (Left _)  -> error $ "expected function id at " ++ showPos pos
    Just (Right e) -> e
    Nothing        -> error $ "unbound variable at " ++ showPos pos

lookupFun :: AlexPosn -> Id -> Context -> Fun
lookupFun pos id s = let (f, _) = lookupFunEntry pos id s in f

lookupFunScope :: AlexPosn -> Id -> Context -> [Id]
lookupFunScope pos id s = let (_, scope) = lookupFunEntry pos id s in scope

-- Unroll loops in a command
unroll :: Integer -> Com -> Com
unroll n (CSeq c1 c2)   = CSeq (unroll n c1) (unroll n c2)
unroll n (CIf b c1 c2)  = CIf b (unroll n c1) (unroll n c2)
unroll n c@(CWhile _ _) = unrollLoop n c
  where unrollLoop 0 _                    = CSkip
        unrollLoop n loop@(CWhile b body) =
          CIf b (CSeq body (unrollLoop (n-1) loop)) CSkip
unroll _ c = c
  
-- Make phi nodes for merging conditional branches
makePhis :: AST -> Context -> Context -> Context -> CompileM Context
makePhis b original_ctx left_ctx right_ctx =
  foldM go original_ctx $ keys original_ctx
  where go :: Context -> Id -> CompileM Context
        go ctx id = do
          var <- Z3.mkFreshIntVar (show id)
          ite <- Z3.mkIte b (lookupZ3Var dummypos id left_ctx)
            (lookupZ3Var dummypos id right_ctx)
          Z3.assert =<< Z3.mkEq var ite
          return $ add id (Left var) ctx

-- Create a context with fresh Z3 variables for each Id
mkInitCtx :: [Id] -> CompileM Context
mkInitCtx init_ids = do
  ctx <- foldM (\acc id -> do
                   var <- Z3.mkFreshIntVar (show id)
                   return $ add id (Left var) acc) empty init_ids
  return ctx

-----------------------------------------
-- Compile an arithmetic expression to Z3

compileAExp :: Context -> AExp -> CompileM AST
compileAExp _ (ANum n) = Z3.mkInteger n
compileAExp ctx (AVar pos id) =
  return $ lookupZ3Var pos id ctx
compileAExp ctx (ACall pos fid args) = do
  let (f, f_scope) = lookupFunEntry pos fid ctx
  let funEntries = map (\fid -> lookupFunEntry pos fid ctx) f_scope
  args' <- mapM (compileAExp ctx) args
  out_var <- Z3.mkFreshIntVar "f_out"
  zero <- Z3.mkInteger 0
  Z3.assert =<< Z3.mkEq out_var zero
  let init_ctx = if elem (out_of f) (params_of f)
                 then empty
                 else (add (out_of f) (Left out_var) empty)
  let ctx' = foldr (\(param, arg) acc ->
                      add param (Left arg) acc)
             init_ctx
             (zip (params_of f) args')
  let ctx'' = foldr (\(fid, funEntry) acc -> add fid (Right funEntry) acc)
        ctx' (zip f_scope funEntries)
  (bound, _) <- get
  let unrolled_body = unroll bound (body_of f)
  ctx''' <- compileCom ctx'' unrolled_body
  return $ lookupZ3Var pos (out_of f) ctx'''
compileAExp ctx (APlus a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkAdd [a1', a2']
compileAExp ctx (AMinus a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkSub [a1', a2']
compileAExp ctx (AMult a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkMul [a1', a2']
compileAExp ctx (ADiv a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkDiv a1' a2'

-------------------------------------
-- Compile a boolean expression to Z3

compileBExp :: Context -> BExp -> CompileM AST
compileBExp _ BTrue = Z3.mkBool True
compileBExp _ BFalse = Z3.mkBool False
compileBExp ctx (BLt a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkLt a1' a2'
compileBExp ctx (BLe a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkLe a1' a2'
compileBExp ctx (BEq a1 a2) = do
  a1' <- compileAExp ctx a1
  a2' <- compileAExp ctx a2
  Z3.mkEq a1' a2'
compileBExp ctx (BOr b1 b2) = do
  b1' <- compileBExp ctx b1
  b2' <- compileBExp ctx b2
  Z3.mkOr [b1', b2']
compileBExp ctx (BAnd b1 b2) = do
  b1' <- compileBExp ctx b1
  b2' <- compileBExp ctx b2
  Z3.mkAnd [b1', b2']
compileBExp ctx (BNot b) = do
  Z3.mkNot =<< compileBExp ctx b

--------------------------
-- Compile a command to Z3

compileCom :: Context -> Com -> CompileM Context
compileCom ctx CSkip = return ctx
compileCom ctx (CAss id a) = do
  n   <- compileAExp ctx a
  var <- Z3.mkFreshIntVar (show id)
  eq  <- Z3.mkEq n var
  Z3.assert eq
  return $ add id (Left var) ctx
compileCom ctx (CSeq c1 c2) = do
  ctx' <- compileCom ctx c1
  compileCom ctx' c2
compileCom ctx (CIf b c1 c2) = do
  b'    <- compileBExp ctx b
  ctx'  <- compileCom ctx c1
  ctx'' <- compileCom ctx c2
  makePhis b' ctx ctx' ctx''
compileCom _ (CWhile _ _) =
  error "Loops must be unrolled before compiling"

---------------------------------------
-- Compile a verification command to Z3

compileVCom :: Context -> VCom -> CompileM Context
compileVCom ctx VCSkip = return ctx
compileVCom ctx (VCIntros ids) =
  foldM (\acc id -> do
            var <- Z3.mkFreshIntVar (show id)
            return $ add id (Left var) acc)
    ctx ids
compileVCom ctx (VCAssume b) = do
  Z3.assert =<< compileBExp ctx b
  return ctx
compileVCom ctx (VCAssert b) = do
  (n, assertions) <- get
  assertion <- compileBExp ctx b
  put (n, assertion : assertions)
  return ctx
compileVCom ctx (VCBound n) = do
  (_, assertions) <- get
  put (n, assertions)
  return ctx
compileVCom ctx (VCSeq vc1 vc2) = do
  ctx' <- compileVCom ctx vc1
  compileVCom ctx' vc2

--------------------------
-- Compile a program to Z3

compileProg :: Prog -> CompileM ()
compileProg p =
  -- Create initial context with function bindings.
  -- We are sure to use foldl here, so that [keys acc] always refers to
  -- all of the keys added to the ctx thus far. Since the functions are
  -- listed in the order in which they appear in the program, the
  -- function scoping rules are implemented by this.
  let ctx = foldl (\acc f -> add (fid_of f) (Right (f, keys acc)) acc)
              empty (funs_of p) in do
  compileVCom ctx (verify_of p)
  (_, assertions) <- get
  -- negate the conjunction of all of the assertions
  Z3.assert =<< Z3.mkNot =<< Z3.mkAnd assertions
