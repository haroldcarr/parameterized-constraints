{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec where

------------------------------------------------------------------------------
import           Example
------------------------------------------------------------------------------
import           Data.Text
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  (_,s1,w1)   <- runIO $ doIt applyPure "expect Right"
  (_,s1',w1') <- runIO $ doIt applyPure "err"

  describe "applyPure" $ do
    it "expect Right" $ (s1,w1) `shouldBe`
      ("applyPure: pure \"expect Right\"",["useApply: got a Right"])

    it "err" $ (s1',w1') `shouldBe`
      ("err",["useApply: got a Left: applyPure: throwError \"err\""])
      -- ("xxx\"",["xxx"])

  --------------------------------------------------
  (_,s2,w2)   <- runIO $ doIt applyIO "expect Right"
  (_,s2',w2') <- runIO $ doIt applyIO "err"
  (_,s3',w3') <- runIO $ doIt applyIO "IOerr"

  describe "applyIO" $ do
    it "expect Right" $ (s2,w2) `shouldBe`
      ("applyIO: pure \"expect Right\"",["useApply: got a Right"])

    it "err" $ (s2',w2') `shouldBe`
      ("err",["useApply: got a Left: applyIO: throwError \"err\""])

    it "IOerr" $ (s3',w3') `shouldBe`
      ("IOerr",["useApply: got a Left: applyIO IOerr/Left/throwError: IOerr: openFile: does not exist (No such file or directory)"])
