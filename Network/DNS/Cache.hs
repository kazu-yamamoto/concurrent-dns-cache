{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DNS.Cache (
    DNSCacheConf(..)
  , Result(..)
  , withDNSCache
  , DNSCache
  , tryLookup
  , lookup
  , wait
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as B
import Data.Char (isDigit)
import Data.Hashable (hash)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.IP (toHostAddress)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Network.DNS hiding (lookup)
import Network.DNS.Cache.PSQ (PSQ)
import qualified Network.DNS.Cache.PSQ as PSQ
import qualified Network.DNS.Cache.Sync as S
import Network.DNS.Cache.Types
import Network.Socket (HostAddress)
import Prelude hiding (lookup)

----------------------------------------------------------------

data DNSCacheConf = DNSCacheConf {
    resolvConfs    :: [ResolvConf]
  , maxConcurrency :: Int
  -- | Seconds.
  , maxTTL         :: NominalDiffTime
  }

data DNSCache = DNSCache {
    cacheSeeds     :: [ResolvSeed]
  , cacheRef       :: IORef (PSQ Value)
  , cacheActiveRef :: IORef (Map Key S.ActiveVar)
  , cacheConcVar   :: S.ConcVar
  , cacheConcLimit :: Int
  , cacheMaxTTL    :: NominalDiffTime
  }

----------------------------------------------------------------

withDNSCache :: DNSCacheConf -> (DNSCache -> IO a) -> IO a
withDNSCache conf func = do
    seeds <- mapM makeResolvSeed (resolvConfs conf)
    cacheref <- newIORef PSQ.empty
    activeref <- newIORef Map.empty
    lvar <- S.newConcVar
    let cache = DNSCache seeds cacheref activeref lvar maxcon maxttl
    void . forkIO $ prune cacheref
    func cache
  where
    maxcon = maxConcurrency conf
    maxttl = maxTTL conf

----------------------------------------------------------------

lookupCache :: DNSCache -> Domain -> IO (Key, Maybe (Prio, Value))
lookupCache cache dom = do
    psq <- readIORef cacheref
    let !mx = PSQ.lookup key psq
    return (key,mx)
  where
    cacheref = cacheRef cache
    !k = B.toShort dom
    !h = hash dom
    !key = Key h k

----------------------------------------------------------------

tryLookup :: DNSCache -> Domain -> IO (Maybe HostAddress)
tryLookup cache dom = do
    (_, mx) <- lookupCache cache dom
    case mx of
        Nothing    -> return Nothing
        Just (_,v) -> Just <$> rotate v

rotate :: Value -> IO HostAddress
rotate (Value a ref) = do
    let (_, siz) = bounds a
    j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
    let !addr = a ! j
    return addr

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x

----------------------------------------------------------------

lookup :: DNSCache -> Domain -> IO (Either DNSError Result)
lookup _     dom
  | isIPAddr dom            = return $ Right $ Numeric $ tov4 dom
  where
    tov4 = read . BS.unpack
lookup cache dom = do
    (key,mx) <- lookupCache cache dom
    case mx of
        Just (_, v)         -> Right . Hit <$> rotate v
        Nothing -> do
            m <- readIORef activeref
            case Map.lookup key m of
                Just avar -> S.listen avar
                Nothing -> do
                    avar <- S.newActiveVar
                    atomicModifyIORef' activeref $
                        \mp -> (Map.insert key avar mp, ())
                    x <- resolve cache dom
                    !res <- case x of
                        Left e      -> return $ Left e
                        Right addrs -> insert cache key addrs
                    atomicModifyIORef' activeref $
                        \mp -> (Map.delete key mp, ())
                    S.tell avar res
                    return res
  where
    activeref = cacheActiveRef cache

insert :: DNSCache -> Key -> [(HostAddress, TTL)] -> IO (Either DNSError Result)
insert _     _   []                   = return $ Left UnexpectedRDATA
insert cache key addrs@((addr,ttl):_) = do
    !val <- newValue $ map fst addrs
    !tim <- addUTCTime lifeTime <$> getCurrentTime
    atomicModifyIORef' cacheref $ \q -> (PSQ.insert key tim val q, ())
    return $! Right $ Resolved addr
  where
    !lifeTime = min (cacheMaxTTL cache) (fromIntegral ttl)
    cacheref = cacheRef cache

newValue :: [HostAddress] -> IO Value
newValue addrs = do
    ref <- newIORef next
    return $! Value arr ref
  where
    !siz = length addrs
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) addrs

----------------------------------------------------------------

resolve :: DNSCache -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
resolve cache dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    seeds = cacheSeeds cache
    body _ = concResolv seeds dom

waitIncrease :: DNSCache -> IO ()
waitIncrease cache = S.waitIncrease lvar lim
  where
    lvar = cacheConcVar cache
    lim = cacheConcLimit cache

decrease :: DNSCache -> IO ()
decrease cache = S.decrease lvar
  where
    lvar = cacheConcVar cache

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
concResolv seeds dom = withResolvers seeds $ \resolvers -> do
    let actions = map (\res -> lookupRaw res dom A) resolvers
    asyncs <- mapM async actions
    (_,eans) <- waitAnyCancel asyncs
    return $ case eans of
        Left  err -> Left err
        Right ans -> fromDNSFormat ans getHostAddressandTTL
  where
    isA r = rrtype r == A
    unTag (RD_A ip) = ip
    unTag _         = error "unTag"
    toAddr = toHostAddress . unTag . rdata
    hostAddressandTTL r = (toAddr r, rrttl r)
    getHostAddressandTTL = map hostAddressandTTL . filter isA . answer

----------------------------------------------------------------

wait :: DNSCache -> (Int -> Bool) -> IO ()
wait cache cond = S.wait lvar cond
  where
    lvar = cacheConcVar cache

prune :: IORef (PSQ Value) -> IO ()
prune cacheref = forever $ do
    threadDelay 10000000
    tim <- getCurrentTime
    atomicModifyIORef' cacheref $ \p -> (snd (PSQ.atMost tim p), ())

----------------------------------------------------------------

isIPAddr :: Domain -> Bool
isIPAddr hn = length groups == 4 && all ip groups
  where
    groups = BS.split '.' hn
    ip x = BS.length x <= 3
        && BS.all isDigit x
        && read (BS.unpack x) <= (255 :: Int)
