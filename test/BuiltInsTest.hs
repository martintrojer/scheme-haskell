module BuiltInsTest (testBuiltIns) where

import Test.Hspec

import Parser
import BuiltIns
import Interpreter
import Types

eval' :: Env -> String -> (Expr, Env)
eval' env str =
  case parseExpr str of
    Just expr -> eval env expr
    Nothing -> error "Parse error"

evalBase :: String -> (Expr, Env)
evalBase = eval' baseEnv

evalStr :: String -> Expr
evalStr = fst . evalBase

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

  describe "define" $
    it "define" $
      let (res, env) = evalBase "(define kalle (+ 41 1))"
      in do
        res `shouldBe` ENull
        (fst . eval' env $ "kalle") `shouldBe` num 42

  describe "cons" $
    it "cons" $ do
      evalStr "(cons 1 2)" `shouldBe` EComb [num 1, num 2]
      evalStr "(cons 1 (cons 2 3))" `shouldBe` EComb [num 1, num 2, num 3]
      evalStr "(cons 1 (cons 2 (cons 3 4)))" `shouldBe` EComb [num 1, num 2, num 3, num 4]
      evalStr "(cons (cons 1 2) 3)" `shouldBe` EComb [EComb [num 1, num 2], num 3]
      evalStr "(cons \"kalle\" 2)" `shouldBe` EComb [EValue (VStr "kalle"), num 2]

  describe "list" $
    it "list" $ do
      evalStr "(list 1 2)" `shouldBe` EComb [num 1, num 2]
      evalStr "(list 5 (list 1 1) 2)" `shouldBe` EComb [num 5, EComb [num 1, num 1], num 2]
      evalStr "(list 1 \"kalle\")" `shouldBe` EComb [num 1, EValue (VStr "kalle")]
      evalStr "(list)" `shouldBe` EComb []

  describe "append" $
    it "append" $ do
      evalStr "(append (list 1 2))" `shouldBe` EComb [num 1, num 2]
      evalStr "(append (list 1 2) (list 3 4))" `shouldBe` EComb [num 1, num 2, num 3, num 4]

  describe "car" $
    it "car" $ do
      evalStr "(car (list 1 2 3))" `shouldBe` num 1
      evalStr "(car (list (list 1 2)))" `shouldBe` EComb [num 1, num 2]

  describe "cdr" $
    it "cdr" $ do
      evalStr "(cdr (list 1 2 3))" `shouldBe` EComb [num 2, num 3]
      evalStr "(cdr (list 1 (list 1 2)))" `shouldBe` EComb [EComb [num 1, num 2]]
      evalStr "(cdr (list 1))" `shouldBe` EComb []

  describe "null?" $
    it "null?" $ do
      evalStr "(null? (list))" `shouldBe` true
      evalStr "(null? (list 1))" `shouldBe` false
      evalStr "(null? (cdr (list 1)))" `shouldBe` true
