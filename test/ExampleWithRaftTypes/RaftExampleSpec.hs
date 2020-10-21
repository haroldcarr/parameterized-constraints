{-# LANGUAGE OverloadedStrings #-}

module ExampleWithRaftTypes.RaftExampleSpec where

------------------------------------------------------------------------------
import           ExampleWithRaftTypes.RaftExample
------------------------------------------------------------------------------
import           Data.Text
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  runIO $ do
    putStrLn "=============================================================================="
    putStrLn ""
    putStrLn "--------------------------------- mainInt pure -------------------------------"
    mainIntPure
    putStrLn "--------------------------------- mainStr pure -------------------------------"
    mainStr applyPure
    putStrLn "--------------------------------- mainStr IO   -------------------------------"
    mainStr applyIO
  describe "raft" $
    it "example" $ True `shouldBe` True
