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

true = EValue (VBool True)
false = EValue (VBool False)
num v = EValue (VNum v)

testBuiltIns :: IO ()
testBuiltIns = hspec $ do
  describe "aritfns" $ do
    it "+" $ do
      evalStr "(+ 1 1)"       `shouldBe` num 2
      evalStr "(+ 1 (+ 1 1))" `shouldBe` num 3
    it "-" $ do
      evalStr "(- 5 3 1)"     `shouldBe` num 1
      evalStr "(- 1 (- 2 3))" `shouldBe` num 2
    it "*" $
      evalStr "(* 5 3 1)"     `shouldBe` num 15
    it "/" $
      evalStr "(/ 10 2 2)"    `shouldBe` num 2.5

  describe "compfns" $ do
    it "=" $ do
      evalStr "(= 1)"         `shouldBe` true
      evalStr "(= 1 1)"       `shouldBe` true
      evalStr "(= 2 (+ 1 1))" `shouldBe` true
      evalStr "(= 1 1 2)"     `shouldBe` false

    it ">" $ do
      evalStr "(> 1 2)"       `shouldBe` false
      evalStr "(> 2 2)"       `shouldBe` false
      evalStr "(> 2 1)"       `shouldBe` true

    it "<" $ do
      evalStr "(< 2 1)"       `shouldBe` false
      evalStr "(< 2 2)"       `shouldBe` false
      evalStr "(< 1 2)"       `shouldBe` true

    it ">=" $ do
      evalStr "(>= 1 2)"      `shouldBe` false
      evalStr "(>= 2 2)"      `shouldBe` true
      evalStr "(>= 2 1)"      `shouldBe` true

    it "<=" $ do
      evalStr "(<= 2 1)"      `shouldBe` false
      evalStr "(<= 2 2)"      `shouldBe` true
      evalStr "(<= 1 2)"      `shouldBe` true

  describe "not" $
    it "not" $ do
      evalStr "(not (= 1 1))" `shouldBe` false
      evalStr "(not (= 1 2))" `shouldBe` true

  describe "if" $
    it "if" $ do
      evalStr "(if (< 2 1) 10 11)"                  `shouldBe` num 11
      evalStr "(if (< (+ 1 1 1) 1) 11 (* 2 5))"     `shouldBe` num 10
      evalStr "(if true 1)"                         `shouldBe` num 1
      evalStr "(if false 1)"                        `shouldBe` ENull

  describe "cond" $
    it "cond" $ do
      evalStr "(cond (true 1) ((= 1 2) 2))"         `shouldBe` num 1
      evalStr "(cond ((= 1 2) 1) (true 2))"         `shouldBe` num 2
      evalStr "(cond (false 1) (false 2) (else 3))" `shouldBe` num 3
      evalStr "(cond (false 1) (false 2))"          `shouldBe` ENull
