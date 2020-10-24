{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OldStuff.Example2Spec where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Text                as T
import           Data.Text.IO             as T
import           GHC.Exts                 (Constraint)
import           Test.Hspec

default (Text)
------------------------------------------------------------------------------

type ApplyFn1 m a = a        -> ExceptT Text m a
type ApplyFn2 m a = a -> Int -> ExceptT Text m a

data Apply2 (c1 :: Constraint) (c2 :: Constraint) (m :: * -> *) (a :: *)
   = Apply2 { ap1 :: c1 => ApplyFn1 m a
            , ap2 :: c2 => ApplyFn2 m a }

{-# ANN doIt2 ("HLint: ignore Eta reduce"::String) #-}
doIt2 :: (c1, c2, Monad m) => Apply2 c1 c2 m a -> a -> m (a, [Text])
doIt2 apply a = do
  (_,s,w) <- runRWST (useApply2 apply) () a
  pure (s,w)

useApply2
 :: (c1, c2, Monad m)
 => Apply2 c1 c2 m a
 -> RWST () [Text] a m ()
useApply2 (Apply2 apply1 apply2) = do
  a1   <- get -- USE THE INITIAL STATE AS INPUT TO BOTH FUNCTIONS
  res1 <- lift $ runExceptT (apply1 a1)
  case res1 of
    Left  l ->
      tell ["useApply2/apply1: got a Left: " <> l]
    Right r -> do
      put r
      tell ["useApply2/apply1: got a Right"]
  -------------------------
  res2 <- lift $ runExceptT (apply2 a1 3)
  case res2 of
    Left  l ->
      tell ["useApply2/apply2: got a Left: " <> l]
    Right r -> do
      put r
      tell ["useApply2/apply2: got a Right"]

twoFuns :: Monad m => Apply2 (MonadPlus m) (MonadIO m) m Text
twoFuns  = Apply2
  (\x -> do
      guard (x /= "IOerr")
      case x of
        "err" -> throwError "apply2/pure: throwError \"err\""
        z     -> pure     $ "apply2/pure: pure \"" <> z <> "\"")
  (\x _i -> do
      liftIO (T.putStrLn ("**************** apply2 " <> x <> " ******************"))
      case x of
        "IOerr" -> do
          xxx <- liftIO $ try (T.readFile (T.unpack x))
          case xxx of
            Right r ->
              pure ("IOerr/Right/pure " <> r)
            Left  (e::SomeException) ->
              throwError $ "apply2 IOerr/Left/throwError: " <> T.pack (show e)
        "err" -> throwError "apply2: throwError \"err\""
        z     -> pure     $ "apply2: pure \"" <> z <> "\"")

------------------------------------------------------------------------------
spec :: Spec
spec  = do
  (s1  ,w1  ) <- runIO $ doIt2 twoFuns "expect Right"
  (s1' ,w1' ) <- runIO $ doIt2 twoFuns "err"
  (s1'',w1'') <- runIO $ doIt2 twoFuns "IOerr"

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
