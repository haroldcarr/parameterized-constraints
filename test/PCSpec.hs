{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module PCSpec where

------------------------------------------------------------------------------
import           PC
------------------------------------------------------------------------------
import           Control.Monad          (MonadPlus, guard)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              as T
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

f1 :: MonadIO m => Function m Text Int
f1 t = do
  liftIO (print t)
  pure (read (T.unpack t))

f2 :: (MonadPlus m, Num a, Ord a, Show a) => Function m (a,a) Text
f2 (n1, n2) = do
  guard (n1 > n2)
  pure (T.pack (show (n1 + n2)))

twoFs :: Functions (m :: * -> *)
                   '[ '( '[ MonadIO m ]                        ,  Text,  Int)
                    , '( '[ MonadPlus m, Num a, Ord a, Show a ], (a,a) , Text)
                    ]
twoFs  = Functions (FunctionWithConstraints f1, (FunctionWithConstraints f2, ()))
