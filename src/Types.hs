module Types(Value(..), Expr(..), Env(..)) where

import Control.Monad.Trans.State
import Data.Map

data Value
  = VNum Double
  | VStr String
  deriving (Eq)

data Expr
  = EValue Value
  | ESymbol String
  | EComb [Expr]

  | EProc (Env -> [Expr] -> (Env, Expr))

instance Show Value where
  show (VNum v) = show v
  show (VStr s) = show s

instance Show Expr where
  show (EValue v)  = show v
  show (ESymbol v) = "#" ++ v
  show (EComb vs)  = show vs
  show (EProc _)   = "Fn"

instance Eq Expr where
  EValue v1  == EValue v2  = v1 == v2
  ESymbol s1 == ESymbol s2 = s1 == s2
  EComb c1   == EComb c2   = c1 == c2
  _          == _          = False

newtype Env = Env { getEnv :: Map String Expr}
