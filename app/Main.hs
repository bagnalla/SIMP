module Main where

import           Control.Monad
import           System.Environment (getArgs)
import qualified Z3.Monad as Z3

import           Ast
import           Compile (compileProg)
import           EvalZ3  (checkSat, evalCompileM)
import           Interp  (interpProg)
import           Parser  (parseProg)

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then error "Error: need input file" else do
    let file = head args
    -- let p_args = [read arg :: Integer | arg <- tail args]

    -- Read in source file
    src <- readFile file

    -- Parse
    case parseProg file src of
      Left s -> error s
      Right p -> do
        
        -- Verify
        let verify = compileProg p
         -- default loop bound = 10, no assertions
        sol <- evalCompileM (10, []) (checkSat verify)
        putStrLn sol
        
        -- Evaluate
        let result = interpProg p
        putStrLn $ show result
