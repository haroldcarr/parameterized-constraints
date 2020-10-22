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
  applyPure2Spec
  applyIO2Spec

applyPure2Spec :: Spec
applyPure2Spec  = do
  ((_,s1 ,w1 ),(_,s2 ,w2 )) <- runIO $ doIt2 applyPure2 "expect Right"
  ((_,s1',w1'),(_,s2',w2')) <- runIO $ doIt2 applyPure2 "err"

  describe "applyPure2" $ do
    it "expect Right" $ ((s1,w1),(s2,w2)) `shouldBe`
      ( ("applyPure2: pure \"expect Right\"",["useApply1: got a Right"])
      , ("applyPure2: pure \"expect Right\"",["useApply1: got a Right"])
      )

    it "err" $ ((s1',w1'),(s2',w2')) `shouldBe`
      ( ("err",["useApply1: got a Left: applyPure2: throwError \"err\""])
      , ("err",["useApply1: got a Left: applyPure2: throwError \"err\""])
      )
      -- ("xxx\"",["xxx"])

applyIO2Spec :: Spec
applyIO2Spec  = do
  --------------------------------------------------
  ((_,s1  ,w1   ),(_,s2   ,w2  )) <- runIO $ doIt2 applyIO2 "expect Right"
  ((_,s1' ,w1'  ),(_,s2'  ,w2' )) <- runIO $ doIt2 applyIO2 "err"
  ((_,s1'',w1'' ),(_,s2'' ,w2'')) <- runIO $ doIt2 applyIO2 "IOerr"

  describe "applyIO2" $ do
    it "expect Right" $ ((s1,w1),(s2,w2)) `shouldBe`
      ( ("applyIO2: pure \"expect Right\"",["useApply1: got a Right"])
      , ("applyIO2: pure \"expect Right\"",["useApply1: got a Right"])
      )

    it "err" $ ((s1',w1'),(s2',w2')) `shouldBe`
      ( ("err",["useApply1: got a Left: applyIO2: throwError \"err\""])
      , ("err",["useApply1: got a Left: applyIO2: throwError \"err\""])
      )

    it "IOerr" $ ((s1'',w1''),(s2'',w2'')) `shouldBe`
      ( ("IOerr",["useApply1: got a Left: applyIO2 IOerr/Left/throwError: IOerr: openFile: does not exist (No such file or directory)"])
      , ("IOerr",["useApply1: got a Left: applyIO2 IOerr/Left/throwError: IOerr: openFile: does not exist (No such file or directory)"])
      )
