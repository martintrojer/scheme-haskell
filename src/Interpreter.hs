module Interpreter(eval) where

import qualified Control.Monad.Trans.State as S
import qualified Data.Map as M
import qualified Data.Functor.Identity as I
import Types

evalExpr :: Expr -> S.State Env Expr
evalExpr v@(EValue _) = return v
evalExpr (ESymbol s) = do
  env <- S.get
  case M.lookup s $ getEnv env of
    Nothing -> error $ "Unknown symbol '" ++ s ++ "'"
    Just res -> evalExpr res
evalExpr p@(EProc _) = return p

evalExpr (EComb (x:xs)) = do
  ex <- evalExpr x
  case ex of
    (EProc fn) -> do
      env <- S.get
      let (res, newEnv) = fn env xs
      S.put newEnv
      return res
    _ -> error "Invalid combination"

evalExpr e = error $ "Invalid state " ++ show e

eval :: Env -> Expr -> (Expr, Env)
eval env expr = I.runIdentity $ S.runStateT (evalExpr expr) env
