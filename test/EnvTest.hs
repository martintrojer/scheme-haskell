module EnvTest (testEnv) where

import qualified Data.Map as M
import Test.Hspec

import Types
import Env

emptyEnv :: Env
emptyEnv = Env [M.empty]

testEnv :: IO ()
testEnv = hspec $ do
  describe "addFrame" $
    it "addFrame" $
      addFrame emptyEnv `shouldBe` Env [M.empty, M.empty]

  describe "dropFrame" $
    it "dropFrame" $
      dropFrame emptyEnv `shouldBe` Env []

  describe "addEntry" $
    it "addEntry" $ do
      addEntry "foo" ENull [M.empty] `shouldBe` [M.fromList [("foo", ENull)]]
      addEntry "foo" ENull [M.fromList [("bar", ESymbol "bar")]] `shouldBe` [M.fromList [("foo", ENull)
                                                                                        ,("bar", ESymbol "bar")]]

  describe "envLookup" $
    it "envLookup" $ do
      envLookup "foo" [M.fromList [("foo", ENull)]] `shouldBe` ENull
      envLookup "foo" [M.empty, M.fromList [("foo", ENull)]] `shouldBe` ENull
      envLookup "foo" [M.fromList [("foo", ENull)], M.empty] `shouldBe` ENull
      envLookup "foo" [M.fromList [("foo", ESymbol "foo")]
                      ,M.fromList [("foo", ENull)]] `shouldBe` ESymbol "foo"
