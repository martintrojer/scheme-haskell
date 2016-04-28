module ParserTest (testParser) where

import Test.Hspec

import Types
import Parser

testParser :: IO ()
testParser = hspec $ do
  describe "Values" $ do
    it "VNum" $
      parseValue "42.42" `shouldBe` (Just $ VNum 42.42)
    it "VStr" $
      parseValue "\"foo\"" `shouldBe` (Just $ VStr "foo")
    it "VBool" $ do
      parseValue "true" `shouldBe` (Just $ VBool True)
      parseValue "false" `shouldBe` (Just $ VBool False)

  describe "Expressions" $ do
    it "EValue VNum" $
      parseExpr "42.42" `shouldBe` (Just $ EValue (VNum 42.42))
    it "EValue VStr" $
      parseExpr "\"foobar\"" `shouldBe` (Just $ EValue (VStr "foobar"))
    it "ESymbol" $
      parseExpr "foobar" `shouldBe` (Just $ ESymbol "foobar")
    it "EComb" $ do
      parseExpr "(1 two \"three\" (false))" `shouldBe`
        (Just $ EComb [EValue (VNum 1.0), ESymbol "two", EValue (VStr "three"), EComb [EValue (VBool False)]])

      parseExpr "(define a 1)" `shouldBe` (Just $ EComb [ESymbol "define", ESymbol "a", EValue (VNum 1)])
