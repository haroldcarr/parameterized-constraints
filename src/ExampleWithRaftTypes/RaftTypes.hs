{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module ExampleWithRaftTypes.RaftTypes where

------------------------------------------------------------------------------
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Data.ByteString
------------------------------------------------------------------------------

type ApplyFn m a = ASAPD a -> Command -> ExceptT RaftAppSignal m (ASAPD a, CommandResult)

type Raft m a = RWST (RaftEnv m a) () (RaftState a) m

data RaftSpec m a = RaftSpec
  { _initialAppState :: ASAPD a
  , _sendMessages    :: [(NodeId,ByteString)] -> m ()
  }

data RaftState a = RaftState
  { _nodeRole                  :: !Role
  , _term                      :: !Term
  , _currentLeader             :: !(Maybe NodeId)
  , _lRecentAndTentativeStates :: !(RATs a)
  } deriving Show
mkRaftState   :: ASAPD a -> RaftState a
mkRaftState a  = RaftState Leader (Term 0) (Just (NodeId 0)) (AppStates [a] [])
mkRaftStateE  :: RaftState a
mkRaftStateE   = RaftState Leader (Term 0) (Just (NodeId 0)) (AppStates [] [])

data RaftEnv m a = RaftEnv
  { _clusterSize :: !Int
  , _quorumSize  :: !Int
  , _rs          :: !(RaftSpec (Raft m a) a)
  }
mkRaftEnv :: Monad m => ASAPD a -> RaftEnv m a
mkRaftEnv a = RaftEnv 5 4 (RaftSpec a (\_ -> return ()))

data ASAPD a = ASAPD
  { _asapdState    :: !a
  , _asapdPlatData :: !CommandPlatformData
  } deriving Show
mkASAPD :: a -> ASAPD a
mkASAPD a = ASAPD a (CommandPlatformData 'c')

newtype CommandPlatformData = CommandPlatformData
  { _cmdpdSeed     :: Char
  } deriving Show

data AppStates a = AppStates
  { apstRecentPostStates :: [a]
  , apstRecords          :: [(Int, a)]
  } deriving (Eq, Show)

type RATs a = AppStates (ASAPD a)

class IsJunoAppState a where
  summarize :: a -> a

instance IsJunoAppState Int where
  summarize x = x*x

instance IsJunoAppState [Char] where
  summarize x = x++x

newtype RaftAppSignal = RASMissingPage Int                                deriving (Eq, Show)
data    Role          = Leader | Follower | Candidate                     deriving (Eq, Show)

newtype Term          = Term          { unTerm          ::     Int      } deriving Show
newtype NodeId        = NodeId        { unNodeId        :: Int          } deriving Show
newtype Command       = Command       { _cmdEntry       :: CommandEntry } deriving (Show, Eq)
newtype CommandEntry  = CommandEntry  { unCommandEntry  :: ByteString   } deriving (Show, Eq, Ord)
newtype CommandResult = CommandResult { unCommandResult :: ByteString   } deriving (Show, Eq, Ord)

mkCommand :: ByteString -> Command
mkCommand = Command . CommandEntry
-------------------------
class RNodeRole s a where
  rNodeRole' :: s a -> Role
instance RNodeRole RaftState a where
  rNodeRole'   = _nodeRole
class WNodeRole s a where
  wNodeRole' :: Role -> s a -> s a
instance WNodeRole RaftState a where
  wNodeRole' x s = s {_nodeRole = x}

type RWNodeRole s a = (RNodeRole s a, WNodeRole s a)

rNodeRole :: (RNodeRole  s a, MonadState (s a) m) => m Role
rNodeRole  = gets rNodeRole'

wNodeRole :: (WNodeRole s a, MonadState (s a) m) => Role -> m ()
wNodeRole x = get >>= put . wNodeRole' x

rmwNodeRole
  :: (RWNodeRole s a, MonadState (s a) m)
  => (Role -> Role)
  -> m ()
rmwNodeRole f = (f <$> rNodeRole) >>= wNodeRole

-------------------------
class RLRecentAndTentativeStates s a where
  rLRecentAndTentativeStates' :: s a -> RATs a
instance RLRecentAndTentativeStates RaftState a where
  rLRecentAndTentativeStates'   = _lRecentAndTentativeStates
class WLRecentAndTentativeStates s a where
  wLRecentAndTentativeStates' :: RATs a -> s a -> s a
instance WLRecentAndTentativeStates RaftState a where
  wLRecentAndTentativeStates' x s = s {_lRecentAndTentativeStates = x}

type RWLRecentAndTentativeStates s a =
  (RLRecentAndTentativeStates s a, WLRecentAndTentativeStates s a)

rLRecentAndTentativeStates
  :: (RLRecentAndTentativeStates s a, MonadState (s a) m)
  => m (RATs a)
rLRecentAndTentativeStates  = gets rLRecentAndTentativeStates'

wLRecentAndTentativeStates
  :: (WLRecentAndTentativeStates s a, MonadState (s a) m)
  => RATs a
  -> m ()
wLRecentAndTentativeStates x = get >>= put . wLRecentAndTentativeStates' x

rmwLRecentAndTentativeStates
  :: (RWLRecentAndTentativeStates s a, MonadState (s a) m)
  => (RATs a -> RATs a)
  -> m ()
rmwLRecentAndTentativeStates f = (f <$> rLRecentAndTentativeStates) >>= wLRecentAndTentativeStates

-------------------------
class ROClusterSize r a where
  roClusterSize' :: r a -> Int
instance ROClusterSize (RaftEnv m) a where
  roClusterSize'   = _clusterSize

roClusterSize :: (ROClusterSize r a, MonadReader (r a) m) => m Int
roClusterSize  = asks roClusterSize'

-------------------------
class ROQuorumSize r a where
  roQuorumSize' :: r a -> Int
instance ROQuorumSize (RaftEnv m) a where
  roQuorumSize'   = _quorumSize

roQuorumSize :: (ROQuorumSize r a, MonadReader (r a) m) => m Int
roQuorumSize  = asks roQuorumSize'

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = void $ runRWST ma r s
