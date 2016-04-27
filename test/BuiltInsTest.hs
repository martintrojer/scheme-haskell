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
    it "+" $ do
      evalStr "(+ 1 1)" `shouldBe` EValue (VNum 2)
      evalStr "(+ 1 (+ 1 1))" `shouldBe` EValue (VNum 3)
    it "-" $ do
      evalStr "(- 5 3 1)" `shouldBe` EValue (VNum 1)
      evalStr "(- 1 (- 2 3))" `shouldBe` EValue (VNum 2)
    it "*" $
      evalStr "(* 5 3 1)" `shouldBe` EValue (VNum 15)
    it "/" $
      evalStr "(/ 10 2 2)" `shouldBe` EValue (VNum 2.5)

  describe "compfns" $ do
    it "=" $ do
      evalStr "(= 1)" `shouldBe` EValue (VBool True)
      evalStr "(= 1 1)" `shouldBe` EValue (VBool True)
      evalStr "(= 2 (+ 1 1))" `shouldBe` EValue (VBool True)
      evalStr "(= 1 1 2)" `shouldBe` EValue (VBool False)

    it ">" $ do
      evalStr "(> 1 2)" `shouldBe` EValue (VBool True)
      evalStr "(> 2 2)" `shouldBe` EValue (VBool False)
      evalStr "(> 2 1)" `shouldBe` EValue (VBool False)

    it "<" $ do
      evalStr "(< 2 1)" `shouldBe` EValue (VBool True)
      evalStr "(< 2 2)" `shouldBe` EValue (VBool False)
      evalStr "(< 1 2)" `shouldBe` EValue (VBool False)

    it ">=" $ do
      evalStr "(>= 1 2)" `shouldBe` EValue (VBool True)
      evalStr "(>= 2 2)" `shouldBe` EValue (VBool True)
      evalStr "(>= 2 1)" `shouldBe` EValue (VBool False)

    it "<=" $ do
      evalStr "(<= 2 1)" `shouldBe` EValue (VBool True)
      evalStr "(<= 2 2)" `shouldBe` EValue (VBool True)
      evalStr "(<= 1 2)" `shouldBe` EValue (VBool False)
