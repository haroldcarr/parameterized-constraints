{-# LANGUAGE OverloadedStrings #-}

module ExampleVSpec where

------------------------------------------------------------------------------
import           ExampleV
------------------------------------------------------------------------------
import           Data.Text
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  i1 <- runIO $ doIx'         Here'  twoFs 1 -- "expect Right"
  i2 <- runIO $ doIx' (There' Here') twoFs 2 -- "err"
  aa <- runIO $ doAll   (SC (SC SN)) twoFs 3
  --(s1'',w1'') <- runIO $ doIt2 twoFuns "IOerr"

  describe "twoFuns" $ do
    it "expect Right" $ i1 `shouldBe` 1

    it "err" $ i2 `shouldBe` 2

    it "IOerr" $ aa `shouldBe` (3,(3,()))
