module BuiltIns (baseEnv) where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map as M
import Interpreter
import Types

--- +, -, *, /

aritFn :: (Double -> Double -> Double) -> [Expr] -> State Env Expr
aritFn fn exprs = do
  xs <- mapM evalExpr exprs
  let res =
        case xs of
          (EValue(VNum first):rest) ->
            foldl (\acc v ->
                    case v of
                      EValue(VNum w) -> fn acc w
                      _              -> aritError)
            first rest
          _ -> aritError
  return . EValue . VNum $ res
  where aritError = error "arithmetic error"

--- =, >, <, <=, >=

compFn :: (Double -> Double -> Bool) -> [Expr] -> State Env Expr
compFn fn exprs = do
  xs <- mapM evalExpr exprs
  let res =
        case xs of
          (EValue(VNum first):rest) ->
            foldl (\(acc, prev) v ->
                    case v of
                      EValue(VNum w) -> (acc && fn prev w, w)
                      _              -> compError)
            (True, first) rest
          _ -> compError
  return . EValue . VBool . fst $ res
  where compError = error "comparison error"

illegalArguments :: String -> t
illegalArguments fn = error $ "Illegal arguments " ++ fn

--- not

notFn :: [Expr] -> State Env Expr
notFn [expr] = do
  x <- evalExpr expr
  let result =
        case x of
          EValue(VBool w) -> not w
          _               -> illegalArguments "not"
  return . EValue . VBool $ result
notFn _ = illegalArguments "not"

--- if

runIf :: Expr -> Expr -> Maybe Expr -> State Env Expr
runIf cond pos negm = do
  x <- evalExpr cond
  case (x, negm) of
    (EValue(VBool True), _)         -> evalExpr pos
    (EValue(VBool False), Just neg) -> evalExpr neg
    (EValue(VBool False), Nothing)  -> return ENull
    _                               -> illegalArguments "if predicate must return bool"

ifFn :: [Expr] -> State Env Expr
ifFn [condExpr, posExpr, negExpr] = runIf condExpr posExpr $ Just negExpr
ifFn [condExpr, posExpr]          = runIf condExpr posExpr Nothing
ifFn _                            = illegalArguments "if"

-- cond

condFn :: [Expr] -> State Env Expr
condFn (EComb c:rest) =
  case c of
    [ESymbol "else", pos] -> evalExpr pos
    [cond, pos] -> do
      res <- evalExpr cond
      case res of
        EValue(VBool True)  -> evalExpr pos
        EValue(VBool False) -> condFn rest
        _ -> illegalArguments "cond predicate must return bool"
    _ -> illegalArguments "cond requires pairs of 2"
condFn [] = return ENull
condFn _  = illegalArguments "cond"

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
