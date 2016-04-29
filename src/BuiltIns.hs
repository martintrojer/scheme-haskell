module BuiltIns (baseEnv) where

import Control.Monad.Trans.State
import qualified Data.Map as M
import Interpreter
import Types
import Env

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

--- cond

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

--- cons

consFn :: [Expr] -> State Env Expr
consFn [e1, e2] = do
  x1 <- evalExpr e1
  x2 <- evalExpr e2
  case (x1, x2) of
    (ex1, ENull) -> return . EComb $ [ex1]
    (ex1, EComb xs) -> return . EComb $ ex1 : xs
    (ex1, ex2) -> return . EComb $ [ex1, ex2]
consFn _ = illegalArguments "cons"

--- list

listFn :: [Expr] -> State Env Expr
-- listFn [EComb []] = return ENull
listFn exprs = do
  xs <- mapM evalExpr exprs
  return . EComb $ xs

--- append

doAppend :: [Expr] -> [Expr]
doAppend [] = []
doAppend (EComb l : rest) = l ++ doAppend rest
doAppend _ = illegalArguments "append"

appendFn :: [Expr] -> State Env Expr
appendFn exprs = do
  xs <- mapM evalExpr exprs
  return . EComb $ doAppend xs

--- car

carFn :: [Expr] -> State Env Expr
carFn [ex] = do
  x <- evalExpr ex
  case x of
    EComb (h:_) -> return h
    _           -> illegalArguments "car"
carFn _ = illegalArguments "car"

--- cdr

cdrFn :: [Expr] -> State Env Expr
cdrFn [ex] = do
  x <- evalExpr ex
  case x of
    EComb (_:rest) -> return . EComb $ rest
    _              -> illegalArguments "cdr"
cdrFn _ = illegalArguments "cdr"

--- null?

nullFn :: [Expr] -> State Env Expr
nullFn [ex] = do
  x <- evalExpr ex
  case x of
    EComb [] -> return . EValue . VBool $ True
    _        -> return . EValue . VBool $ False
nullFn _ = illegalArguments "null?"

--- define

defFn :: [Expr] -> State Env Expr
-- vals, lambdas
defFn [ESymbol name, ex] = do
  x <- evalExpr ex
  modify $ Env . addEntry name x . getEnv
  return ENull
-- functions
defFn (EComb (ESymbol name : params) : body) = do
  modify $ Env . addEntry name (EFunc params body) . getEnv
  return ENull
defFn e = illegalArguments $ "define" ++ show e

--- let

doBinds :: [Expr] -> State Env Expr
doBinds (EComb c : rest) = do
  _ <- defFn c
  doBinds rest
doBinds _ = return ENull

letFn :: [Expr] -> State Env Expr
letFn [EComb binds, body] = do
  modify addFrame
  _ <- doBinds binds
  res <- evalExpr body
  modify dropFrame
  return res
letFn _ = illegalArguments "let"

--- lambda

lambdaFn :: [Expr] -> State Env Expr
lambdaFn (EComb params : body) = return $ EFunc params body
lambdaFn _ = illegalArguments "lambda"

-------------------------------------------------------------------

baseEnv :: Env
baseEnv = Env [M.fromList [ ("+",      EProc $ aritFn (+))
                          , ("-",      EProc $ aritFn (-))
                          , ("*",      EProc $ aritFn (*))
                          , ("/",      EProc $ aritFn (/))

                          , ("=",      EProc $ compFn (==))
                          , (">",      EProc $ compFn (>))
                          , ("<",      EProc $ compFn (<))
                          , (">=",     EProc $ compFn (>=))
                          , ("<=",     EProc $ compFn (<=))

                          , ("not",    EProc notFn)
                          , ("if",     EProc ifFn)
                          , ("cond",   EProc condFn)

                          , ("cons",   EProc consFn)
                          , ("list",   EProc listFn)
                          , ("append", EProc appendFn)
                          , ("car",    EProc carFn)
                          , ("cdr",    EProc cdrFn)
                          , ("null?",  EProc nullFn)

                          , ("define", EProc defFn)
                          , ("lambda", EProc lambdaFn)
                          , ("let",    EProc letFn)

                          , ("begin",  EProc evalExprs)
                          ]]
