{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache.Cache (
    CacheRef
  , newCacheRef
  , lookupCacheRef
  , insertCacheRef
  , pruneCacheRef
  , newKey
  ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as B
import Data.Hashable (hash)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Network.DNS.Cache.PSQ (PSQ)
import qualified Network.DNS.Cache.PSQ as PSQ
import Network.DNS.Cache.Types

newtype CacheRef = CacheRef (IORef (PSQ Entry))

newCacheRef :: IO CacheRef
newCacheRef = CacheRef <$> newIORef PSQ.empty

lookupCacheRef :: Key -> CacheRef -> IO (Maybe (Prio, Entry))
lookupCacheRef key (CacheRef ref) = PSQ.lookup key <$> readIORef ref

insertCacheRef :: Key -> Prio -> Entry -> CacheRef -> IO ()
insertCacheRef key tim ent (CacheRef ref) =
    atomicModifyIORef' ref $ \q -> (PSQ.insert key tim ent q, ())

pruneCacheRef :: Prio -> CacheRef -> IO ()
pruneCacheRef tim (CacheRef ref) =
    atomicModifyIORef' ref $ \p -> (snd (PSQ.atMost tim p), ())

newKey :: ByteString -> Key
newKey dom = Key h k
  where
    !k = B.toShort dom
    !h = hash dom
