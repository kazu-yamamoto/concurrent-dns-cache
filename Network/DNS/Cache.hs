{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | DNS cache to resolve domains concurrently.

module Network.DNS.Cache (
    DNSCacheConf(..)
  , DNSCache
  , withDNSCache
  -- * Looking up
  , lookup
  , lookupCache
  -- * Resolving
  , Result(..)
  , resolve
  , resolveCache
  -- * Waiting
  , wait
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BS
import Data.IP (toHostAddress)
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Network.DNS hiding (lookup)
import Network.DNS.Cache.Cache
import qualified Network.DNS.Cache.Sync as S
import Network.DNS.Cache.Types
import Network.DNS.Cache.Utils
import Network.DNS.Cache.Value
import Prelude hiding (lookup)

----------------------------------------------------------------

-- | Configuration for DNS cache.
data DNSCacheConf = DNSCacheConf {
  -- | A list of resolvers (cache DNS servers).
  --   A domain is resolved by the resolvers concurrently.
  --   The first reply is used regardless of success/failure at this moment
    resolvConfs    :: [ResolvConf]
  -- | Capability of how many domains can be resolved concurrently
  , maxConcurrency :: Int
  -- | The minimum bound of cache duration for success replies in seconds.
  , minTTL         :: NominalDiffTime
  -- | The maximum bound of cache duration for success replies in seconds.
  , maxTTL         :: NominalDiffTime
  -- | The cache duration for failure replies in seconds.
  , negativeTTL    :: NominalDiffTime
  }

-- | An abstract data for DNS cache.
--   Cached domains are expired every 10 seconds according to their TTL.
data DNSCache = DNSCache {
    cacheSeeds      :: [ResolvSeed]
  , cacheNofServers :: !Int
  , cacheRef        :: CacheRef
  , cacheActiveRef  :: S.ActiveRef
  , cacheConcVar    :: S.ConcVar
  , cacheConcLimit  :: Int
  , cacheMinTTL     :: NominalDiffTime
  , cacheMaxTTL     :: NominalDiffTime
  , cacheNegTTL     :: NominalDiffTime
  }

----------------------------------------------------------------

-- | A basic function to create DNS cache.
--   Domains should be resolved in the function of the second argument.
withDNSCache :: DNSCacheConf -> (DNSCache -> IO a) -> IO a
withDNSCache conf func = do
    seeds <- mapM makeResolvSeed (resolvConfs conf)
    let n = length seeds
    cacheref <- newCacheRef
    activeref <- S.newActiveRef
    lvar <- S.newConcVar
    let cache = DNSCache seeds n cacheref activeref lvar maxcon minttl maxttl negttl
    bracket (forkIO $ prune cacheref) killThread (const $ func cache)
  where
    maxcon = maxConcurrency conf
    minttl = minTTL conf
    maxttl = maxTTL conf
    negttl = negativeTTL conf

----------------------------------------------------------------

lookupPSQ :: DNSCache -> Domain -> IO (Key, Maybe (Prio, Entry))
lookupPSQ cache dom = do
    !mx <- lookupCacheRef key cacheref
    return (key,mx)
  where
    cacheref = cacheRef cache
    !key = newKey dom

----------------------------------------------------------------

-- | Lookup 'Domain' only in the cache.
lookupCache :: DNSCache -> Domain -> IO (Maybe HostAddress)
lookupCache cache dom = do
    mx <- resolveCache cache dom
    case mx of
        Nothing -> return Nothing
        Just ev -> return (fromEither ev)

----------------------------------------------------------------

-- | Lookup 'Domain' in the cache.
--   If not exist, queries are sent to DNS servers and
--   resolved IP addresses are cached.
lookup :: DNSCache -> Domain -> IO (Maybe HostAddress)
lookup cache dom = fromEither <$> resolve cache dom

----------------------------------------------------------------

-- | Lookup 'Domain' only in the cache.
resolveCache :: DNSCache -> Domain -> IO (Maybe (Either DNSError Result))
resolveCache _ dom
  | isIPAddr dom       = Just . Right . Numeric <$> return (tov4 dom)
  where
    tov4 = toHostAddress . read . BS.unpack
resolveCache cache dom = do
    (_, mx) <- lookupPSQ cache dom
    case mx of
        Nothing           -> return Nothing
        Just (_, Right v) -> Just . Right . Hit <$> rotate v
        Just (_, Left e)  -> Just . Left <$> return e

-- | Lookup 'Domain' in the cache.
--   If not exist, queries are sent to DNS servers and
--   resolved IP addresses are cached.
resolve :: DNSCache -> Domain -> IO (Either DNSError Result)
resolve _     dom
  | isIPAddr dom            = return $ Right $ Numeric $ tov4 dom
  where
    tov4 = toHostAddress . read . BS.unpack
resolve cache dom = do
    (key,mx) <- lookupPSQ cache dom
    case mx of
        Just (_,ev) -> case ev of
            Left  e -> Left <$> return e
            Right v -> Right . Hit <$> rotate v
        Nothing -> do
            -- If this domain is being resolved by another thread
            -- let's wait.
            ma <- S.lookupActiveRef key activeref
            case ma of
                Just avar -> S.listen avar
                Nothing   -> do
                    avar <- S.newActiveVar
                    S.insertActiveRef key avar activeref
                    x <- sendQuery cache dom
                    !res <- case x of
                        Left  err   -> insertNegative cache key err
                        Right []    -> insertNegative cache key UnexpectedRDATA
                        Right addrs -> insertPositive cache key addrs
                    S.deleteActiveRef key activeref
                    S.tell avar (toHit res)
                    return res
  where
    activeref = cacheActiveRef cache
    toHit (Right (Resolved addr)) = Right (Hit addr)
    toHit x                       = x

insertPositive :: DNSCache -> Key -> [(HostAddress, TTL)]
               -> IO (Either DNSError Result)
insertPositive _     _   []                   = error "insertPositive"
insertPositive cache key addrs@((addr,ttl):_) = do
    !ent <- positiveEntry $ map fst addrs
    !tim <- addUTCTime lifeTime <$> getCurrentTime
    insertCacheRef key tim ent cacheref
    return $! Right $ Resolved addr
  where
    minttl = cacheMinTTL cache
    maxttl = cacheMaxTTL cache
    !lifeTime = minttl `max` (maxttl `min` fromIntegral ttl)
    cacheref = cacheRef cache

insertNegative :: DNSCache -> Key -> DNSError -> IO (Either DNSError Result)
insertNegative cache key err = do
    !tim <- addUTCTime lifeTime <$> getCurrentTime
    insertCacheRef key tim (Left err) cacheref
    return $ Left err
  where
    lifeTime = cacheNegTTL cache
    cacheref = cacheRef cache

----------------------------------------------------------------

sendQuery :: DNSCache -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
sendQuery cache dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    body _ = concResolv cache dom

waitIncrease :: DNSCache -> IO ()
waitIncrease cache = S.waitIncrease lvar lim
  where
    lvar = cacheConcVar cache
    lim = cacheConcLimit cache

decrease :: DNSCache -> IO ()
decrease cache = S.decrease lvar
  where
    lvar = cacheConcVar cache

concResolv :: DNSCache -> Domain -> IO (Either DNSError [(HostAddress,TTL)])
concResolv cache dom = withResolvers seeds $ \resolvers -> do
    eans <- resolv n resolvers dom
    return $ case eans of
        Left  err -> Left err
        Right ans -> fromDNSFormat ans getHostAddressandTTL
  where
    n = cacheNofServers cache
    seeds = cacheSeeds cache
    isA r = rrtype r == A
    unTag (RD_A ip) = ip
    unTag _         = error "unTag"
    toAddr = toHostAddress . unTag . rdata
    hostAddressandTTL r = (toAddr r, rrttl r)
    getHostAddressandTTL = map hostAddressandTTL . filter isA . answer

resolv :: Int -> [Resolver] -> Domain -> IO (Either DNSError DNSFormat)
resolv 1 resolvers dom = lookupRaw (head resolvers) dom A
resolv _ resolvers dom = do
    asyncs <- mapM async actions
    snd <$> waitAnyCancel asyncs
  where
    actions = map (\res -> lookupRaw res dom A) resolvers

----------------------------------------------------------------

-- | Wait until the predicate in the second argument is satisfied.
--   The predicate are given the number of the current resolving domains.
--
-- For instance, if you ensure that no resolvings are going on:
--
-- > wait cache (== 0)
--
-- If you want to ensure that capability of concurrent resolving is not full:
--
-- > wait cache (< maxCon)
--
-- where 'maxCon' represents 'maxConcurrency' in 'DNSCacheConf'.
wait :: DNSCache -> (Int -> Bool) -> IO ()
wait cache cond = S.wait lvar cond
  where
    lvar = cacheConcVar cache

prune :: CacheRef -> IO ()
prune cacheref = forever $ do
    threadDelay 10000000
    tim <- getCurrentTime
    pruneCacheRef tim cacheref
