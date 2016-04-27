module BuiltIns where

import qualified Data.Map as M
import Interpreter
import Types

aritFn :: (Double -> Double -> Double) -> Env -> [Expr] -> (Env, Expr)
aritFn fn env exprs = (env, result)
  where aritError = error "arithmetic error"
        result =
          case map (fst . eval env) exprs of
            (EValue(VNum first):vs) ->
              EValue . VNum $ foldl (\acc v ->
                                      case v of
                                        EValue(VNum w) -> fn acc w
                                        _ -> aritError) first vs
            _ -> aritError

compFn :: (Double -> Double -> Bool) -> Env -> [Expr] -> (Env, Expr)
compFn fn env exprs = (env, result)
  where compError = error "comparison error"
        result =
          case map (fst . eval env) exprs of
            (EValue(VNum first):vs) ->
              EValue . VBool . fst $ foldl (\(acc, prev) v ->
                                             case v of
                                               EValue(VNum w) -> (acc && fn w prev, w)
                                               _ -> compError) (True, first) vs
            _ -> compError

baseEnv :: Env
baseEnv = Env $ M.fromList [ ("+",  EProc $ aritFn (+))
                           , ("-",  EProc $ aritFn (-))
                           , ("*",  EProc $ aritFn (*))
                           , ("/",  EProc $ aritFn (/))

                           , ("=",  EProc $ compFn (==))
                           , (">",  EProc $ compFn (>))
                           , ("<",  EProc $ compFn (<))
                           , (">=", EProc $ compFn (>=))
                           , ("<=", EProc $ compFn (<=))

                           ]
