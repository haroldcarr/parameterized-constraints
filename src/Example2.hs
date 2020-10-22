{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Example2 where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Text                as T
import           GHC.Exts                 (Constraint)
------------------------------------------------------------------------------

-- see test/ExampleSpec.hs for usage

------------------------------------------------------------------------------

type ApplyFn1 m a = a -> ExceptT Text m a
type ApplyFn2 m a = a -> ExceptT Text m a

data Apply2 (c1 :: Constraint) (c2 :: Constraint) (m :: * -> *) (a :: *)
   = Apply2 { ap1 :: c1 => ApplyFn1 m a
            , ap2 :: c2 => ApplyFn1 m a
            }

{-# ANN doIt2 ("HLint: ignore Eta reduce"::String) #-}
doIt2 :: (c1, c2, Monad m) => Apply2 c1 c2 m a -> a -> m ( ((), a, [Text])
                                                         , ((), a, [Text]) )
doIt2 apply a = do
  one <- runRWST (useApply2 apply) "one" a
  two <- runRWST (useApply2 apply) "two" a
  pure (one, two)

useApply2
 :: (c1, c2, Monad m)
 => Apply2 c1 c2 m a
 -> RWST Text [Text] a m ()
useApply2 (Apply2 apply1 apply2) = do
  a1   <- get
  res1 <- lift $ runExceptT (apply1 a1)
  case res1 of
    Left  l ->
      tell ["useApply2/apply1: got a Left: " <> l]
    Right r -> do
      put r
      tell ["useApply2/apply1: got a Right"]
  -------------------------
  a2   <- get
  res2 <- lift $ runExceptT (apply2 a2)
  case res2 of
    Left  l ->
      tell ["useApply2/apply2: got a Left: " <> l]
    Right r -> do
      put r
      tell ["useApply2/apply2: got a Right"]

applyPure2 :: Monad m => Apply2 ()          () m Text
applyPure2  = Apply2 (\case
                        "err" -> throwError "applyPure2: throwError \"err\""
                        x     -> pure     $ "applyPure2: pure \"" <> x <> "\"")
                     (\case
                        "err" -> throwError "applyPure2: throwError \"err\""
                        x     -> pure     $ "applyPure2: pure \"" <> x <> "\"")

applyIO2   ::            Apply2 (MonadIO m) (MonadIO m) m Text
applyIO2    = Apply2
 (\x -> do
  liftIO (putStrLn "**************** applyIO2 ******************")
  case x of
    "IOerr" -> do
       xxx <- liftIO $ try (T.pack <$> readFile (T.unpack x))
       case xxx of
         Right r ->
           pure ("IOerr/Right/pure " <> r)
         Left  (e::SomeException) ->
           throwError $ "applyIO2 IOerr/Left/throwError: " <> T.pack (show e)
    "err" -> throwError "applyIO2: throwError \"err\""
    z     -> pure     $ "applyIO2: pure \"" <> z <> "\"")
 (\x -> do
  liftIO (putStrLn "**************** applyIO2 ******************")
  case x of
    "IOerr" -> do
       xxx <- liftIO $ try (T.pack <$> readFile (T.unpack x))
       case xxx of
         Right r ->
           pure ("IOerr/Right/pure " <> r)
         Left  (e::SomeException) ->
           throwError $ "applyIO2 IOerr/Left/throwError: " <> T.pack (show e)
    "err" -> throwError "applyIO2: throwError \"err\""
    z     -> pure     $ "applyIO2: pure \"" <> z <> "\"")


