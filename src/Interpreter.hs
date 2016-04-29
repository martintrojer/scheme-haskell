module Interpreter(evalExpr, evalExprs, eval) where

import Control.Monad
import qualified Control.Monad.Trans.State as S
import qualified Data.Functor.Identity as I
import Types
import Env

evalExpr :: Expr -> S.State Env Expr

evalExpr v@(EValue _) = return v

evalExpr (ESymbol s) = do
  env <- S.get
  return . envLookup s $ getEnv env

evalExpr (EComb (ex:rest)) = do
  x <- evalExpr ex
  case x of
    (EProc fn)          -> fn rest
    (EFunc params body) -> do
      S.modify addFrame
      forM_ (zip params rest) $ \(ESymbol name, y) -> do
        z <- evalExpr y
        S.modify $ Env . addEntry name z . getEnv
      res <- evalExprs body
      S.modify dropFrame
      return res
    _                   -> error "Invalid combination"


evalExpr e = error $ "Invalid state " ++ show e

evalExprs :: [Expr] -> S.State Env Expr
evalExprs [] = return ENull
evalExprs [e] = evalExpr e
evalExprs (e:rest) = do
  _ <- evalExpr e
  evalExprs rest

eval :: Env -> Expr -> (Expr, Env)
eval env expr = I.runIdentity $ S.runStateT (evalExpr expr) env
