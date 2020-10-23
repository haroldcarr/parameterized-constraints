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
  o1 <- runIO $ runNthFunction           IZ  twoFs "1" -- "expect Right"
  o2 <- runIO $ runNthFunction       (IS IZ) twoFs (2,1::Int) -- "err"
  oo <- runIO $ runAllFunctions (SC (SC SN)) twoFs ("1", ((2,1::Int), ()))
  --(s1'',w1'') <- runIO $ doIt2 twoFuns "IOerr"

  describe "twoFuns" $ do
    it "expect Right" $ o1 `shouldBe` 1

    it "err" $ o2 `shouldBe` "3"

    it "IOerr" $ oo `shouldBe` (1, ("3", ()))
