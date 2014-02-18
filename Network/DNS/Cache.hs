{-# LANGUAGE BangPatterns #-}

module Network.DNS.Cache (
    DNSCacheConf(..)
  , Lookup
  , Wait
  , withDNSCache
  ) where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (newTVarIO, atomically, readTVar, writeTVar, modifyTVar', check)
import Control.Exception (bracket)
import Data.Array
import qualified Data.ByteString.Short as B
import Data.Hashable (hash)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.IP (IPv4)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Network.BSD (HostName)
import Network.DNS
import Network.DNS.Cache.Types
import qualified Data.PSQueue as PSQ

withDNSCache :: DNSCacheConf -> (Lookup -> Wait -> IO a) -> IO a
withDNSCache conf func = do
    cref <- newIORef PSQ.empty
    lvar <- newTVarIO 0
    let cache = DNSCache cref lvar (maxConcurrency conf) (lifeTime conf)
    seeds <- makeSeeds (dnsServers conf)
    func (lookupHostAddress cache seeds) (wait cache)

makeSeeds :: [HostName] -> IO [ResolvSeed]
makeSeeds ips = mapM (makeResolvSeed . toConf) ips
 where
   toConf ip = defaultResolvConf { resolvInfo = RCHostName ip }

lookupHostAddress :: DNSCache -> [ResolvSeed] -> Lookup
lookupHostAddress cache seeds dom = do
    psq <- readIORef cref
    case PSQ.lookup key psq of
        Just (Value _ a ref) -> do
            putStrLn "hit!"
            let (_, siz) = bounds a
            j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
            let !addr = a ! j
            return $ Right addr
        Nothing -> do
            x <- resolve cache seeds dom
            case x of
                Left e           -> return $ Left e
                Right []         -> return $ Left UnexpectedRDATA
                Right ips@(ip:_) -> do
                    !val <- newValue cache ips
                    atomicModifyIORef' cref $
                        \q -> (PSQ.insert key val q, ())
                    return $ Right ip
  where
    !k = B.toShort dom
    !h = hash dom
    !key = Key h k
    cref = cacheref cache

newValue :: DNSCache -> [IPv4] -> IO Value
newValue (DNSCache _ _ _ lf) ips = do
    ref <- newIORef next
    tim <- getCurrentTime
    let !tim' = addUTCTime lf tim
    return $! Value tim' arr ref
  where
    !siz = length ips
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) ips

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x

resolve :: DNSCache -> [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
resolve cache seeds dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    body _ = concResolv seeds dom

wait :: DNSCache -> IO ()
wait (DNSCache _ lvar lim _) = atomically $ do
    x <- readTVar lvar
    check (x < lim)

waitIncrease :: DNSCache -> IO ()
waitIncrease (DNSCache _ lvar lim _) = atomically $ do
    x <- readTVar lvar
    check (x < lim)
    let !x' = x + 1
    writeTVar lvar x'

decrease :: DNSCache -> IO ()
decrease (DNSCache _ lvar _ _) = atomically $ modifyTVar' lvar (subtract 1)

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
concResolv seeds dom = withResolvers seeds $ \resolvers -> do
    let actions = map (`lookupA` dom) resolvers
    asyncs <- mapM async actions
    (_,x) <- waitAnyCancel asyncs
    return x
