{-# LANGUAGE OverloadedStrings #-}

module Example2Spec where

------------------------------------------------------------------------------
import           Example2
------------------------------------------------------------------------------
import           Data.Text
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  (s1  ,w1   ) <- runIO $ doIt2 twoFuns "expect Right"
  (s1' ,w1'  ) <- runIO $ doIt2 twoFuns "err"
  (s1'',w1'' ) <- runIO $ doIt2 twoFuns "IOerr"

  describe "twoFuns" $ do
    it "expect Right" $ (s1,w1) `shouldBe`
      ( "apply2: pure \"expect Right\""
      , ["useApply2/apply1: got a Right"
        ,"useApply2/apply2: got a Right"])

    it "err" $ (s1',w1') `shouldBe`
      ( "err"
      , ["useApply2/apply1: got a Left: apply2/pure: throwError \"err\""
        ,"useApply2/apply2: got a Left: apply2: throwError \"err\""])

    it "IOerr" $ (s1'',w1'') `shouldBe`
      ( "IOerr"
      , ["useApply2/apply1: got a Left: " -- the space is mzero for Text
        ,"useApply2/apply2: got a Left: apply2 IOerr/Left/throwError: IOerr: openFile: does not exist (No such file or directory)"])
