module BuiltIns where

import qualified Data.Map as M
import Interpreter
import Types

aritError = error "arithmetic error"

aritFn :: (Double -> Double -> Double) -> Env -> [Expr] -> (Env, Expr)
aritFn fn env exprs = (env, result)
  where result =
          case map (fst . eval env) exprs of
            (EValue(VNum v):vs) ->
              EValue . VNum $ foldl (\acc w ->
                                      case w of
                                        EValue(VNum x) -> fn acc x
                                        _ -> aritError) v vs
            _ -> aritError


baseEnv :: Env
baseEnv = Env $ M.fromList [ ("+", EProc $ aritFn (+))
                           , ("-", EProc $ aritFn (-))
                           , ("*", EProc $ aritFn (*))
                           , ("/", EProc $ aritFn (/))
                           ]
