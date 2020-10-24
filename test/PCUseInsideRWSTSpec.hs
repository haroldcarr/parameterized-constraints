{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PCUseInsideRWSTSpec where

------------------------------------------------------------------------------
import           PC
------------------------------------------------------------------------------
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Text                as T
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

f1 :: MonadIO m => Function m Text Int
f1 t = do
  liftIO (print t)
  pure (read (T.unpack t))

fFun :: Functions (m :: * -> *) '[ '( '[ MonadIO m ], Text, Int ) ]
fFun  = Functions (FunctionWithConstraints f1, ())

main :: IO ()
main  = do
  (_,s,w) <- top fFun "main"
  print w
  print s

{-# ANN top ("HLint: ignore Eta reduce"::String) #-}
top
  :: (AllConstraints cios, MonadIO m)
  => Functions m cios
  -> Text
  -> m ((), Text, [Text])
top funs a = runRWST (topM funs) "X" a

topM
 :: (AllConstraints cios, Monad m)
 => Functions m cios
 -> RWST Text [Text] a m ()
topM funs = do
  --let res = runNthFunction IZ funs "InputTo" -- ***** RIGHT HERE *****
  pure ()

spec :: Spec
spec  = do
  x <- runIO main
  describe "spec" $
    it "stub" $ x `shouldBe` ()
