{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (newTVarIO, atomically, TVar, readTVar, writeTVar, retry, modifyTVar')
import Control.Exception (bracket)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.IP (IPv4)
import Network.BSD (HostName)
import Network.DNS
import Data.Time.Clock
import Data.Array
import Network.DNS.Cache.Types
import qualified Data.ByteString.Short as B
import Data.Hashable

import qualified Data.PSQueue as PSQ

newtype Limit = Limit (TVar Int)

withDNSCache :: DNSCacheConf -> (Lookup -> IO a) -> IO a
withDNSCache conf func = do
    cacheref <- newIORef PSQ.empty
    let cache = DNSCache cacheref
    limvar <- newTVarIO 0
    let lim = Limit limvar
    seeds <- makeSeeds (dnsServers conf)
    func (lookupHostAddress cache lim seeds)

makeSeeds :: [HostName] -> IO [ResolvSeed]
makeSeeds ips = mapM (makeResolvSeed . toConf) ips
 where
   toConf ip = defaultResolvConf { resolvInfo = RCHostName ip }

lookupHostAddress :: DNSCache -> Limit -> [ResolvSeed] -> Lookup
lookupHostAddress (DNSCache cacheref) lim seeds dom = do
    let k = B.toShort dom
        h = hash dom
        key = Key h k
    psq <- readIORef cacheref
    case PSQ.lookup key psq of
        Just (Value _ a ref) -> do
            putStrLn "hit!"
            let (_, siz) = bounds a
            j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
            let !addr = a ! j
            return $ Right addr
        Nothing -> do
            x <- tryResolve lim seeds dom
            case x of
                Left e    -> return $ Left e
                Right []  -> return $ Left UnexpectedRDATA
                Right ips -> do
                    let siz = length ips
                        !next = adjust 0 siz
                    ref <- newIORef next
                    tim <- getCurrentTime
                    let tim' = addUTCTime 300 tim -- fixme
                        arr = listArray (0,siz-1) ips
                        val = Value tim' arr ref
                        ip = head ips
                    atomicModifyIORef' cacheref $ \c -> (PSQ.insert key val c, ())
                    return $ Right ip

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = (i + 1) `mod` n

tryResolve :: Limit -> [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
tryResolve lim seeds dom = bracket setup teardown body
  where
    setup = wait lim 200 -- fixme
    teardown _ = release lim
    body _ = concResolv seeds dom

wait :: Limit -> Int -> IO ()
wait (Limit limvar) limit = atomically $ do
    x <- readTVar limvar
    if x < limit then do
        let !x' = x + 1
        writeTVar limvar x'
      else
        retry

release :: Limit -> IO ()
release (Limit limvar) = atomically $ modifyTVar' limvar (subtract 1)

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
concResolv seeds dom = withResolvers seeds $ \resolvers -> do
    let actions = map (flip lookupA dom) resolvers
    asyncs <- mapM async actions
    (_,x) <- waitAnyCancel asyncs
    return x
