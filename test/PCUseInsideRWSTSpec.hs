{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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

f2 :: Monad   m => Function m Int  Int
f2 i = pure (i * 2)

fFun :: Functions (m :: * -> *) '[ '( '[ MonadIO m ], Text, Int )
                                 , '( '[ Monad   m ], Int , Int )
                                 ]
fFun  = Functions (FunctionWithConstraints f1, (FunctionWithConstraints f2, ()))

type CIOS cios n1 n2 =
  ( NthConstraints n1 cios, GetInputs (Lookup n1 cios) ~ Text, GetOutput (Lookup n1 cios) ~ Int
  , NthConstraints n2 cios, GetInputs (Lookup n2 cios) ~ Int , GetOutput (Lookup n2 cios) ~ Int )

main :: IO (Text, [Text])
main  = do
  (_,s,w) <- top fFun (IZ, IS IZ)
  pure (s, w)

top
  :: (MonadIO m, CIOS cios n1 n2)
  => Functions m cios
  -> (Ix n1 cios, Ix n2 cios)
  -> m ((), Text, [Text])
top funs ixs = runRWST (topM ixs) funs "initial state"

topM
  :: (MonadIO m, CIOS cios n1 n2)
  => (Ix n1 cios, Ix n2 cios)
  -> RWST (Functions m cios) [Text] Text m ()
topM ixs = do
  useF1 ixs
  useF2ViaIntermediary ixs

useF1
  :: (Monad m, CIOS cios n1 n2)
  => (Ix n1 cios, Ix n2 cios)
  -> RWST (Functions m cios) [Text] Text m ()
useF1 (ix1, _) = do
  s <- get
  tell ["useF1", "state", s]
  funs <- ask
  r <- lift $ T.pack . show <$> runNthFunction ix1 funs "45"
  tell ["useF1", "result", r]
  put r

useF2ViaIntermediary
  :: (Monad m, CIOS cios n1 n2)
  => (Ix n1 cios, Ix n2 cios)
  -> RWST (Functions m cios) [Text] Text m ()
useF2ViaIntermediary ixs = do
  tell ["intermediary"]
  useF2 ixs

useF2
  :: ( Monad m
     , NthConstraints n2 cios, GetInputs (Lookup n2 cios) ~ Int , GetOutput (Lookup n2 cios) ~ Int)
  => (Ix n1 cios, Ix n2 cios)
  -> RWST (Functions m cios) [Text] Text m ()
useF2 (_, ix2) = do
  s <- get
  tell ["useF2", "state", s]
  funs <- ask
  r <- lift $ T.pack . show <$> runNthFunction ix2 funs 2
  tell ["useF2", "result", r]
  put r
  pure ()

spec :: Spec
spec  = do
  x <- runIO main
  describe "PCUseInsideRWSTSpec" $
    it "main" $ x `shouldBe`
    ("4", ["useF1","state","initial state","useF1","result","45"
          ,"intermediary"
          ,"useF2","state","45","useF2","result","4"])
