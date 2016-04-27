module BuiltInsTest (testBuiltIns) where

import Test.Hspec

import Parser
import BuiltIns
import Interpreter
import Types

evalStr :: String -> Expr
evalStr str =
  case parseExpr str of
    Just expr -> fst . eval baseEnv $ expr
    Nothing -> error "Parse error"

testBuiltIns :: IO ()
testBuiltIns = hspec $ do
  describe "aritfns" $ do
    it "+" $
      evalStr "(+ 1 1)" `shouldBe` EValue (VNum 2)
    it "-" $
      evalStr "(- 5 3 1)" `shouldBe` EValue (VNum 1)
    it "*" $
      evalStr "(* 5 3 1)" `shouldBe` EValue (VNum 15)
    it "/" $
      evalStr "(/ 10 2 2)" `shouldBe` EValue (VNum 2.5)
