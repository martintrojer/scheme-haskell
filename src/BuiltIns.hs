module BuiltIns (baseEnv) where

import qualified Data.Map as M
import Interpreter
import Types

evalExpr :: Env -> Expr -> Expr
evalExpr env = fst . eval env

--- +, -, *, /

aritFn :: (Double -> Double -> Double) -> Env -> [Expr] -> (Expr, Env)
aritFn fn env exprs = (EValue . VNum $ result, env)
  where aritError = error "arithmetic error"
        result =
          case map (evalExpr env) exprs of
            (EValue(VNum first):vs) ->
              foldl (\acc v ->
                      case v of
                        EValue(VNum w) -> fn acc w
                        _              -> aritError)
              first vs
            _ -> aritError

--- =, >, <, <=, >=

compFn :: (Double -> Double -> Bool) -> Env -> [Expr] -> (Expr, Env)
compFn fn env exprs = (EValue . VBool . fst $ result, env)
  where compError = error "comparison error"
        result =
          case map (evalExpr env) exprs of
            (EValue(VNum first):vs) ->
              foldl (\(acc, prev) v ->
                      case v of
                        EValue(VNum w) -> (acc && fn prev w, w)
                        _              -> compError)
              (True, first) vs
            _ -> compError

illegalArguments :: String -> t
illegalArguments fn = error $ "Illegal arguments " ++ fn

--- not

notFn :: Env -> [Expr] -> (Expr, Env)
notFn env [expr] = (EValue . VBool $ result, env)
  where result =
          case evalExpr env expr of
            (EValue(VBool v)) -> not v
            _                 -> illegalArguments "not"
notFn _ _ = illegalArguments "not"

--- if

runIf :: Env -> Expr -> Expr -> Maybe Expr -> (Expr, Env)
runIf env cond pos (Just neg) =
  case evalExpr env cond of
    (EValue(VBool True))  -> eval env pos
    (EValue(VBool False)) -> eval env neg
    _                     -> illegalArguments "if"
runIf env cond pos Nothing =
  case evalExpr env cond of
    (EValue(VBool True))  -> eval env pos
    (EValue(VBool False)) -> (ENull, env)
    _                     -> illegalArguments "if"

ifFn :: Env -> [Expr] -> (Expr, Env)
ifFn env [condExpr, posExpr, negExpr] = runIf env condExpr posExpr $ Just negExpr
ifFn env [condExpr, posExpr] = runIf env condExpr posExpr Nothing
ifFn _ _ = illegalArguments "if"

--- cond

runCond :: Env -> [Expr] -> Maybe Expr
runCond env [ESymbol "else", posExpr] = Just . evalExpr env $ posExpr
runCond env [condExpr, posExpr] =
  case evalExpr env condExpr of
    (EValue(VBool True))  -> Just . evalExpr env $ posExpr
    (EValue(VBool False)) -> Nothing
    _                     -> illegalArguments "cond"
runCond _ _ = illegalArguments "cond"

condFn :: Env -> [Expr] -> (Expr, Env)
condFn env ((EComb c):exprs) =
  case runCond env c of
    Just e  -> (e, env)
    Nothing -> condFn env exprs
condFn env _ = (ENull, env)

-------------------------------------------------------------------

baseEnv :: Env
baseEnv = Env $ M.fromList [ ("+",    EProc $ aritFn (+))
                           , ("-",    EProc $ aritFn (-))
                           , ("*",    EProc $ aritFn (*))
                           , ("/",    EProc $ aritFn (/))

                           , ("=",    EProc $ compFn (==))
                           , (">",    EProc $ compFn (>))
                           , ("<",    EProc $ compFn (<))
                           , (">=",   EProc $ compFn (>=))
                           , ("<=",   EProc $ compFn (<=))

                           , ("not",  EProc notFn)
                           , ("if",   EProc ifFn)
                           , ("cond", EProc condFn)
                           ]
