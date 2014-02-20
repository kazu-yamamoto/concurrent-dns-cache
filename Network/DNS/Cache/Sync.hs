{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache.Sync (
    ConcVar
  , newConcVar
  , wait
  , waitIncrease
  , decrease
  , ActiveVar
  , newActiveVar
  , tell
  , listen
  , ActiveRef
  , newActiveRef
  , lookupActiveRef
  , insertActiveRef
  , deleteActiveRef
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Network.DNS.Cache.Types
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)

----------------------------------------------------------------

newtype ConcVar = ConcVar (TVar Int)

newConcVar :: IO ConcVar
newConcVar = ConcVar <$> newTVarIO 0

wait :: ConcVar -> (Int -> Bool) -> IO ()
wait (ConcVar var) cond = atomically $ do
    x <- readTVar var
    check (cond x)

waitIncrease :: ConcVar -> Int -> IO ()
waitIncrease (ConcVar var) lim = atomically $ do
    x <- readTVar var
    check (x < lim)
    let !x' = x + 1
    writeTVar var x'

decrease :: ConcVar -> IO ()
decrease (ConcVar var) = atomically $ modifyTVar' var (subtract 1)

----------------------------------------------------------------

newtype ActiveVar = ActiveVar (TMVar (Either DNSError Result))

newActiveVar :: IO ActiveVar
newActiveVar = ActiveVar <$> newEmptyTMVarIO

tell :: ActiveVar -> Either DNSError Result -> IO ()
tell (ActiveVar var) r = atomically $ putTMVar var r

listen :: ActiveVar -> IO (Either DNSError Result)
listen (ActiveVar var) = atomically $ readTMVar var

----------------------------------------------------------------

newtype ActiveRef = ActiveRef (IORef (Map Key ActiveVar))

newActiveRef :: IO ActiveRef
newActiveRef = ActiveRef <$> newIORef Map.empty

lookupActiveRef :: Key -> ActiveRef -> IO (Maybe ActiveVar)
lookupActiveRef key (ActiveRef ref) = Map.lookup key <$> readIORef ref

insertActiveRef :: Key -> ActiveVar -> ActiveRef -> IO ()
insertActiveRef key avar (ActiveRef ref) =
    atomicModifyIORef' ref $ \mp -> (Map.insert key avar mp, ())

deleteActiveRef :: Key -> ActiveRef -> IO ()
deleteActiveRef key (ActiveRef ref) =
    atomicModifyIORef' ref $ \mp -> (Map.delete key mp, ())
