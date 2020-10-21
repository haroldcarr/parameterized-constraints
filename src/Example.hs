{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Example where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.Text                as T
import           GHC.Exts                 (Constraint)
------------------------------------------------------------------------------

-- see test/ExampleSpec.hs for usage

------------------------------------------------------------------------------

type ApplyFn m a = a -> ExceptT Text m a

newtype Apply (c :: Constraint) (m :: * -> *) (a :: *)
      = Apply { apply :: c => ApplyFn m a }

{-# ANN doIt ("HLint: ignore Eta reduce"::String) #-}
doIt :: (c, Monad m) => Apply c m a -> a -> m ((), a, [Text])
doIt apply0 a = runRWST (useApply apply0) "X" a

useApply
 :: (c, Monad m)
 => Apply c m a
 -> RWST Text [Text] a m ()
useApply (Apply apply0) = do
  a   <- get
  res <- lift $ runExceptT (apply0 a)
  case res of
    Left  l ->
      tell ["useApply: got a Left: " <> l]
    Right r -> do
      put r
      tell ["useApply: got a Right"]

applyPure :: Monad m => Apply ()          m Text
applyPure  = Apply $ \case
  "err" -> throwError "applyPure: throwError \"err\""
  x     -> pure     $ "applyPure: pure \"" <> x <> "\""

applyIO   ::            Apply (MonadIO m) m Text
applyIO    = Apply $ \x -> do
  liftIO (putStrLn "**************** applyIO ******************")
  case x of
    "IOerr" -> do
       xxx <- liftIO $ try (T.pack <$> readFile (T.unpack x))
       case xxx of
         Right r ->
           pure ("IOerr/Right/pure " <> r)
         Left  (e::SomeException) ->
           throwError $ "applyIO IOerr/Left/throwError: " <> T.pack (show e)
    "err" -> throwError "applyIO: throwError \"err\""
    z     -> pure     $ "applyIO: pure \"" <> z <> "\""

-- 'applyIO' cannot be called unless in 'MonadIO'.
-- 'applyPure' can be used in pure code AND 'MonadIO'.
{-
xxxx :: Monad m => m (Text, Text)
xxxx = do
  (_,s1,_w1) <- doIt applyPure ("x"::Text)
  (_,s2,_w2) <- doIt applyIO   ("x"::Text) -- Could not deduce (MonadIO m) arising from a use of ‘doIt’
  return (s1, s2)
-}
