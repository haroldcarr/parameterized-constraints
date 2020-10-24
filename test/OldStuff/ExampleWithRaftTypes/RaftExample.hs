{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module OldStuff.ExampleWithRaftTypes.RaftExample where

------------------------------------------------------------------------------
import           OldStuff.ExampleWithRaftTypes.RaftTypes
------------------------------------------------------------------------------
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           GHC.Exts                       (Constraint)
------------------------------------------------------------------------------
-- for examples
import           Debug.Trace
------------------------------------------------------------------------------

-- see test/ExampleWithRaftTypes/RaftExample.hs for usage

------------------------------------------------------------------------------
newtype Apply (c :: Constraint) (m :: * -> *) (a :: *)
      = Apply { apply :: c => ApplyFn m a }
------------------------------------------------------------------------------
applyPure :: (Monad m, IsJunoAppState a) => Apply ()          m a
applyPure  = Apply $ \asapd cmd ->
  applyAux asapd cmd

applyIO   ::           IsJunoAppState a  => Apply (MonadIO m) m a
applyIO    = Apply $ \asapd cmd -> do
  liftIO (putStrLn "**************** applyIO ******************\n")
  applyAux asapd cmd

applyAux  :: (Monad m, IsJunoAppState a) => ApplyFn m a
applyAux (ASAPD s pd) (Command (CommandEntry x)) =
  if x /= "foo"
    then throwError (RASMissingPage 0)
    else return (ASAPD (summarize s) pd, CommandResult x)
------------------------------------------------------------------------------
runAE     :: ( c, Monad m
             , ROClusterSize r a, ROQuorumSize r a, RWLRecentAndTentativeStates s a )
          => Command
          -> Apply c m a
          -> RWST (r a) String (s a) m ()
runAE cmd (Apply f) = do
  cs <- roClusterSize
  qs <- roQuorumSize
  tell (" cs==" ++ show cs ++ "; qs==" ++ show qs)
  (AppStates ps r) <- rLRecentAndTentativeStates
  lr <- lift $ runExceptT (f (head ps) cmd)
  case lr of
    Left  _ ->
      tell "; got Left"
    Right (asapd', _cr) -> do
      wLRecentAndTentativeStates (AppStates (asapd':ps) r)
      tell "; got Right"
  return ()

runAER    :: Monad m
          => String
          -> RWST (r a) String (s a) m ()
runAER s   = tell $ "runAER " ++ s
-- ==============================================================================
-- framework usage
-- ==============================================================================
-- executables (i.e., PgdCommandHandlerServer.hs)

-- This is the pattern to use in executables.
mainIntPure :: IO ()
mainIntPure  = mainInt applyPure
 where
  mainInt  :: (c, MonadIO m)        => Apply c m Int -> m ()
  mainInt   = server (mkASAPD 3)

-- This is used in local tests (so it can be given a pure or and IO).
mainStr  :: (c, MonadIO m)        => Apply c m String -> m ()
mainStr   = server (mkASAPD "X")

-- Juno.Network.Server.main
server
  :: (c, MonadIO m)
  => ASAPD a
  -> Apply c m a
  -> m ()
server = runJuno

pureHandler :: Bool
pureHandler  = False

-- Juno.Spec.Simple.runJuno
runJuno
  :: (c, MonadIO m)
  => ASAPD a
  -> Apply c m a
  -> m ()
runJuno initAppState applyFn =
  runRWS_
    (if pureHandler then raftServerIO applyFn else raftServer applyFn)
    (mkRaftEnv   initAppState)
    (mkRaftState initAppState)

raftServer
  :: (c, MonadIO m)
  => Apply c m a
  -> Raft m a ()
raftServer = handleEvents

-- Raft works with anymonad 'm'.
-- The client provided 'Apply c m a' also works on top of 'm',
-- but also by client provided properties over 'm' with c.
handleEvents
  :: (c, MonadIO m)
  => Apply c m a
  -> Raft m a ()
handleEvents applyFn = do
  r <- ask
  s <- get
  (_, s',  w ) <- lift $ runRWST (runAE (mkCommand "foo") applyFn) r s
  put s'
  trace ("msgs: " ++ w)  (return ())
  (_, s'', w') <- lift $ runRWST (runAER           "foo")          r s
  put s''
  trace ("msgs: " ++ w') (return ())

------------------------------------------------------------------------------

raftServerIO
  :: (c, MonadIO m)
  => Apply c m a
  -> Raft m a ()
raftServerIO = handleEventsIO

handleEventsIO
  :: (c, MonadIO m)
  => Apply c m a
  -> Raft m a ()
handleEventsIO applyFn = do
  r <- ask
  s <- get
  (_, s',  w ) <- lift $ runRWST (handleEventsPure applyFn) r s
  put s'
  trace ("msgs: " ++ w)  (return ())

handleEventsPure
  :: (c, Monad m, ROClusterSize r a, ROQuorumSize r a, RWLRecentAndTentativeStates s a)
  => Apply c m a
  -> RWST (r a) String (s a) m ()
handleEventsPure applyFn = do
  runAE (mkCommand "foo") applyFn
  runAER           "foo"
