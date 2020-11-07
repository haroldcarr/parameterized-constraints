{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module PC2Spec where

------------------------------------------------------------------------------
import           PC2
------------------------------------------------------------------------------
import           Control.Monad            (MonadPlus, guard)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.RWS.Strict
import           Data.Text                as T
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- user defines "parameterized" functions

f1 :: (MonadIO m, i1 ~ Text, o1 ~ Int) => Function m i1 o1
f1 t = do
  liftIO (print t)
  pure (read (T.unpack t))

f2 :: (MonadPlus m, Num a, Ord a, Show a, i2 ~ (a,a), o2 ~ Text) => Function m i2 o2
f2 (n1, n2) = do
  guard (n1 > n2)
  pure (T.pack (show (n1 + n2)))

twoFs
  :: Functions
     (m :: * -> *)
     '[ '[ MonadIO m, i1 ~ Text, o1 ~ Int ], '[ MonadPlus m, Num a, Ord a, Show a, i2 ~ (a,a), o2 ~ Text ] ]
     '[ i1                                 , i2 ]
     '[ o1                                 , o2 ]
twoFs  = Functions (FunctionWithConstraints f1, (FunctionWithConstraints f2, ()))
{-
-- user pass those functions and indices and the user's top-level entry point
-- to 3rd party library (e.g., PTFD)

main :: IO ()
main  = ptfd twoFs _ topM

------------------------------------------------------------------------------
-- 3rd party library calls back to user's top-level entry point

ptfd
  :: (MonadIO m, AllConstraints css)
  => Functions m css iss os
  -> Ixs
  -> ((Monad m, AllConstraints css)
      => Ixs -- (Ix n1 css iss os, Ix n2 css iss os)
      -> RWST (Functions m css iss os) [Text] Text m ())
  -> m ()
ptfd funs ixs top = do
  runRWST (top ixs) funs "initial state"
  pure ()

-- for integration between user and 3rd party library
data Ixs where
  ICons :: forall n css iss os. Ix n css iss os -> Ixs -> Ixs
  INil  :: Ixs

------------------------------------------------------------------------------
-- user's top level entry point uses the parameterized functions

topM
  :: (Monad m, AllConstraints css)
  => Ixs -- (Ix n1 css iss os, Ix n2 css iss os)
  -> RWST (Functions m css iss os) [Text] Text m ()
topM _ixs =
  useF1 undefined

useF1
  :: (Monad m, AllConstraints css, NthConstraints n1 css)
  => (Ix n1 css iss os, Ix n2 css iss os)
  -> RWST (Functions m css iss os) [Text] Text m ()
useF1 (ix1, _) = do
  s <- get
  tell ["useF1", "state", s]
  funs <- ask
  r <- lift $ T.pack . show <$> runNthFunction ix1 funs "45"
  tell ["useF1", "result", r]
  put r
-}
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  o1 <- runIO $ runNthFunction           IZ  twoFs "1" -- "expect Right"
  o2 <- runIO $ runNthFunction       (IS IZ) twoFs (2,1::Int) -- "err"
  --oo <- runIO $ runAllFunctions (SC (SC SN)) twoFs ("1", ((2,1::Int), ()))
  --(s1'',w1'') <- runIO $ doIt2 twoFuns "IOerr"

  describe "twoFuns" $ do
    it "expect Right" $ o1 `shouldBe` 1

    it "err" $ o2 `shouldBe` "3"

    --it "IOerr" $ oo `shouldBe` (1, ("3", ()))




