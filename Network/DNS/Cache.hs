{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DNS.Cache (
    DNSCacheConf(..)
  , Result(..)
  , withDNSCache
  , DNSCache
  , lookupHostAddress
  , wait
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (newTVarIO, atomically, readTVar, writeTVar, modifyTVar', check, TVar)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as B
import Data.Char (isDigit)
import Data.Hashable (hash)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.IP (toHostAddress)
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Network.BSD (HostName)
import Network.DNS
import Network.DNS.Cache.PSQ (PSQ)
import qualified Network.DNS.Cache.PSQ as PSQ
import Network.DNS.Cache.Types
import Network.Socket (HostAddress)

----------------------------------------------------------------

data DNSCacheConf = DNSCacheConf {
    dnsServers :: [HostName]
  , maxConcurrency :: Int
  , lifeTime :: NominalDiffTime
  -- fixme timeout for dns lib
  -- fixme retries for dns lib
  }

data DNSCache = DNSCache {
    seeds :: [ResolvSeed]
  , cacheref :: IORef (PSQ Value)
  , limvar :: TVar Int
  , limit :: Int
  , life :: NominalDiffTime
  }

data Result = Hit HostAddress
            | Resolved HostAddress
            | Numeric HostAddress
            deriving Show

----------------------------------------------------------------

withDNSCache :: DNSCacheConf -> (DNSCache -> IO a) -> IO a
withDNSCache conf func = do
    ss <- makeSeeds (dnsServers conf)
    cref <- newIORef PSQ.empty
    lvar <- newTVarIO 0
    let cache = DNSCache ss cref lvar (maxConcurrency conf) (lifeTime conf)
    void . forkIO $ prune cref
    func cache

----------------------------------------------------------------

makeSeeds :: [HostName] -> IO [ResolvSeed]
makeSeeds ips = mapM (makeResolvSeed . toConf) ips
 where
   toConf ip = defaultResolvConf { resolvInfo = RCHostName ip }

----------------------------------------------------------------

lookupHostAddress :: DNSCache -> Domain -> IO (Either DNSError Result)
lookupHostAddress _     dom
  | isIPAddr dom            = return $ Right $ Numeric $ tov4 dom
  where
    tov4 = read . BS.unpack
lookupHostAddress cache dom = do
    psq <- readIORef cref
    case PSQ.lookup key psq of
        Just (_, Value a ref) -> do
            let (_, siz) = bounds a
            j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
            let !addr = a ! j
            return $ Right $ Hit addr
        Nothing -> do
            x <- resolve cache dom
            case x of
                Left e           -> return $ Left e
                Right []         -> return $ Left UnexpectedRDATA
                Right addrs@(addr:_) -> do
                    !val <- newValue addrs
                    tim <- addUTCTime lf <$> getCurrentTime
                    atomicModifyIORef' cref $
                        \q -> (PSQ.insert key tim val q, ())
                    return $ Right $ Resolved addr
  where
    !k = B.toShort dom
    !h = hash dom
    !key = Key h k
    cref = cacheref cache
    lf = life cache

newValue :: [HostAddress] -> IO Value
newValue addrs = do
    ref <- newIORef next
    return $! Value arr ref
  where
    !siz = length addrs
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) addrs

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x

----------------------------------------------------------------

resolve :: DNSCache -> Domain -> IO (Either DNSError [HostAddress])
resolve cache dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    body _ = concResolv (seeds cache) dom

waitIncrease :: DNSCache -> IO ()
waitIncrease cache = atomically $ do
    x <- readTVar lvar
    check (x < lim)
    let !x' = x + 1
    writeTVar lvar x'
  where
    lvar = limvar cache
    lim = limit cache

decrease :: DNSCache -> IO ()
decrease (DNSCache _ _ lvar _ _) = atomically $ modifyTVar' lvar (subtract 1)

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [HostAddress])
concResolv ss dom = withResolvers ss $ \resolvers -> do
    -- fixme TTL
    let actions = map (`lookupA` dom) resolvers
    asyncs <- mapM async actions
    (_,eips) <- waitAnyCancel asyncs
    return $ case eips of
        Left e    -> Left e
        Right ips -> Right (map toHostAddress ips)

----------------------------------------------------------------

wait :: DNSCache -> (Int -> Bool) -> IO ()
wait (DNSCache _ _ lvar _ _) cond = atomically $ do
    x <- readTVar lvar
    check (cond x)

prune :: IORef (PSQ Value) -> IO ()
prune cref = forever $ do
    threadDelay 10000000
    tim <- getCurrentTime
    atomicModifyIORef' cref $ \p -> (snd (PSQ.atMost tim p), ())

----------------------------------------------------------------

isIPAddr :: Domain -> Bool
isIPAddr hn = length groups == 4 && all ip groups
  where
    groups = BS.split '.' hn
    ip x = BS.length x <= 3
        && BS.all isDigit x
        && read (BS.unpack x) <= (255 :: Int)
