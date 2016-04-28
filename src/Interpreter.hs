module Interpreter(evalExpr, eval) where

import qualified Control.Monad.Trans.State as S
import qualified Data.Functor.Identity as I
import Types
import Env

evalExpr :: Expr -> S.State Env Expr

evalExpr v@(EValue _) = return v

evalExpr (ESymbol s) = do
  env <- S.get
  return . envLookup s $ getEnv env

evalExpr p@(EProc _) = return p

evalExpr (EComb (x:xs)) = do
  ex <- evalExpr x
  case ex of
    (EProc fn) -> fn xs
    _          -> error "Invalid combination"

evalExpr e = error $ "Invalid state " ++ show e

eval :: Env -> Expr -> (Expr, Env)
eval env expr = I.runIdentity $ S.runStateT (evalExpr expr) env
