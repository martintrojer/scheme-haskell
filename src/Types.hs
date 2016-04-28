module Types(Value(..), Expr(..), Env(..)) where

import Control.Monad.Trans.State
import Data.Map

data Value
  = VNum Double
  | VStr String
  | VBool Bool
  deriving (Eq)

data Expr
  = EValue Value
  | ESymbol String
  | EComb [Expr]
  | ENull

  | EProc ([Expr] -> State Env Expr)

newtype Env = Env { getEnv :: Map String Expr}

instance Show Value where
  show (VNum v)      = show v
  show (VStr s)      = show s
  show (VBool True)  = "#t"
  show (VBool False) = "#f"

instance Show Expr where
  show (EValue v)  = show v
  show ENull       = "nil"
  show (ESymbol v) = "#" ++ v
  show (EComb vs)  = show vs
  show (EProc _)   = "Fn"

instance Eq Expr where
  EValue v1  == EValue v2  = v1 == v2
  ESymbol s1 == ESymbol s2 = s1 == s2
  EComb c1   == EComb c2   = c1 == c2
  ENull      == ENull      = True
  _          == _          = False
