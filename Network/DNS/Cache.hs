{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (newTVarIO, atomically, TVar, readTVar, writeTVar, modifyTVar', check)
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
    psq <- readIORef cacheref
    case PSQ.lookup key psq of
        Just (Value _ a ref) -> do
            putStrLn "hit!"
            let (_, siz) = bounds a
            j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
            let !addr = a ! j
            return $ Right addr
        Nothing -> do
            x <- resolve lim seeds dom
            case x of
                Left e           -> return $ Left e
                Right []         -> return $ Left UnexpectedRDATA
                Right ips@(ip:_) -> do
                    let !val = newValue ips
                    atomicModifyIORef' cacheref $
                        \q -> (PSQ.insert key val q, ())
                    return $ Right ip
  where
    !k = B.toShort dom
    !h = hash dom
    !key = Key h k

newValue :: [IPv4] -> IO Value
newValue ips = do
    ref <- newIORef next
    tim <- getCurrentTime
    let !tim' = addUTCTime 300 tim -- fixme
    return $! Value tim' arr ref
  where
    !siz = length ips
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) ips

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x

resolve :: Limit -> [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
resolve lim seeds dom = bracket setup teardown body
  where
    setup = waitIncrease lim 200 -- fixme
    teardown _ = decrease lim
    body _ = concResolv seeds dom

wait :: Limit -> Int -> IO ()
wait (Limit limvar) limit = atomically $ do
    x <- readTVar limvar
    check (x < limit)

waitIncrease :: Limit -> Int -> IO ()
waitIncrease (Limit limvar) limit = atomically $ do
    x <- readTVar limvar
    check (x < limit)
    let !x' = x + 1
    writeTVar limvar x'

decrease :: Limit -> IO ()
decrease (Limit limvar) = atomically $ modifyTVar' limvar (subtract 1)

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
concResolv seeds dom = withResolvers seeds $ \resolvers -> do
    let actions = map (flip lookupA dom) resolvers
    asyncs <- mapM async actions
    (_,x) <- waitAnyCancel asyncs
    return x
