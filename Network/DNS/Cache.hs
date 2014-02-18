{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.DNS.Cache (
    DNSCacheConf(..)
  , Lookup
  , Wait
  , Result(..)
  , withDNSCache
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (newTVarIO, atomically, readTVar, writeTVar, modifyTVar', check, TVar)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Data.Array
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as B
import Data.Char (isDigit)
import Data.Hashable (hash)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.IP (IPv4)
import Data.Time (getCurrentTime, addUTCTime, NominalDiffTime)
import Network.BSD (HostName)
import Network.DNS
import Network.DNS.Cache.PSQ (PSQ)
import qualified Network.DNS.Cache.PSQ as PSQ
import Network.DNS.Cache.Types

----------------------------------------------------------------

data DNSCacheConf = DNSCacheConf {
    dnsServers :: [HostName]
  , maxConcurrency :: Int
  , lifeTime :: NominalDiffTime
  -- fixme timeout for dns lib
  -- fixme retries for dns lib
  }

data DNSCache = DNSCache {
    cacheref :: IORef (PSQ Value)
  , limvar :: TVar Int
  , limit :: Int
  , life :: NominalDiffTime
  }

data Result = Hit IPv4
            | Resolved IPv4
            | Numeric IPv4
            | IllegalDomain
            | NoA
            | SeqMismatch
            | Timeout
            | Broken
            deriving Show

----------------------------------------------------------------

type Lookup = Domain -> IO Result
type Wait = (Int -> Bool) -> IO ()

withDNSCache :: DNSCacheConf -> (Lookup -> Wait -> IO a) -> IO a
withDNSCache conf func = do
    cref <- newIORef PSQ.empty
    lvar <- newTVarIO 0
    let cache = DNSCache cref lvar (maxConcurrency conf) (lifeTime conf)
    seeds <- makeSeeds (dnsServers conf)
    void . forkIO $ prune cref
    func (lookupHostAddress cache seeds) (wait cache)

----------------------------------------------------------------

makeSeeds :: [HostName] -> IO [ResolvSeed]
makeSeeds ips = mapM (makeResolvSeed . toConf) ips
 where
   toConf ip = defaultResolvConf { resolvInfo = RCHostName ip }

----------------------------------------------------------------

lookupHostAddress :: DNSCache -> [ResolvSeed] -> Lookup
lookupHostAddress _     _     dom
  | isIllegal dom                 = return IllegalDomain
  | isIPAddr dom                  = return $ Numeric $ read $ BS.unpack dom
lookupHostAddress cache seeds dom = do
    psq <- readIORef cref
    case PSQ.lookup key psq of
        Just (_, Value a ref) -> do
            let (_, siz) = bounds a
            j <- atomicModifyIORef' ref $ \i -> (adjust i siz, i)
            let !addr = a ! j
            return $ Hit addr
        Nothing -> do
            x <- resolve cache seeds dom
            case x of
                Left e           -> return $ toError e
                Right []         -> return NoA
                Right ips@(ip:_) -> do
                    !val <- newValue ips
                    tim <- addUTCTime lf <$> getCurrentTime
                    atomicModifyIORef' cref $
                        \q -> (PSQ.insert key tim val q, ())
                    return $ Resolved ip
  where
    !k = B.toShort dom
    !h = hash dom
    !key = Key h k
    cref = cacheref cache
    lf = life cache

newValue :: [IPv4] -> IO Value
newValue ips = do
    ref <- newIORef next
    return $! Value arr ref
  where
    !siz = length ips
    !next = adjust 0 siz
    !arr = listArray (0,siz-1) ips

toError :: DNSError -> Result
toError SequenceNumberMismatch = SeqMismatch
toError TimeoutExpired         = Timeout
toError UnexpectedRDATA        = Broken

adjust :: Int -> Int -> Int
adjust i 0 = i
adjust i n = let !x = (i + 1) `mod` n in x

----------------------------------------------------------------

resolve :: DNSCache -> [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
resolve cache seeds dom = bracket setup teardown body
  where
    setup = waitIncrease cache
    teardown _ = decrease cache
    body _ = concResolv seeds dom

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
decrease (DNSCache _ lvar _ _) = atomically $ modifyTVar' lvar (subtract 1)

concResolv :: [ResolvSeed] -> Domain -> IO (Either DNSError [IPv4])
concResolv seeds dom = withResolvers seeds $ \resolvers -> do
    let actions = map (`lookupA` dom) resolvers
    asyncs <- mapM async actions
    (_,x) <- waitAnyCancel asyncs
    return x

----------------------------------------------------------------

wait :: DNSCache -> Wait
wait (DNSCache _ lvar _ _) cond = atomically $ do
    x <- readTVar lvar
    check (cond x)

prune :: IORef (PSQ Value) -> IO ()
prune cref = forever $ do
    threadDelay 10000000
    tim <- getCurrentTime
    atomicModifyIORef' cref $ \p -> (snd (PSQ.atMost tim p), ())

----------------------------------------------------------------

isIllegal :: Domain -> Bool
isIllegal ""                    = True
isIllegal dom
  | '.' `BS.notElem` dom        = True
  | ':' `BS.elem` dom           = True
  | '/' `BS.elem` dom           = True
  | BS.length dom > 253         = True
  | any (\x -> BS.length x > 63)
        (BS.split '.' dom)      = True
isIllegal _                     = False

isIPAddr :: Domain -> Bool
isIPAddr hn = length groups == 4 && all ip groups
  where
    groups = BS.split '.' hn
    ip x = BS.length x <= 3
        && BS.all isDigit x
        && read (BS.unpack x) <= (255 :: Int)
