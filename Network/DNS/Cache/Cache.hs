{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache.Cache (
    CacheRef
  , newCacheRef
  , lookupCacheRef
  , insertCacheRef
  , pruneCacheRef
  ) where

import Control.Applicative ((<$>))
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Network.DNS.Cache.Types

type PSQ = OrdPSQ
newtype CacheRef = CacheRef (IORef (PSQ Key Prio Entry))

newCacheRef :: IO CacheRef
newCacheRef = CacheRef <$> newIORef PSQ.empty

lookupCacheRef :: Key -> CacheRef -> IO (Maybe (Prio, Entry))
lookupCacheRef key (CacheRef ref) = PSQ.lookup key <$> readIORef ref

insertCacheRef :: Key -> Prio -> Entry -> CacheRef -> IO ()
insertCacheRef key tim ent (CacheRef ref) =
    atomicModifyIORef' ref $ \q -> (PSQ.insert key tim ent q, ())

pruneCacheRef :: Prio -> CacheRef -> IO ()
pruneCacheRef tim (CacheRef ref) =
    atomicModifyIORef' ref $ \p -> (snd (PSQ.atMostView tim p), ())
