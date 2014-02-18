module Network.DNS.Cache.Types where

import Control.Concurrent.STM (TVar)
import Data.Array (Array)
import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.IP
import Data.PSQueue (PSQ)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Network.BSD (HostName)
import Network.DNS (Domain)
-- import Network.Socket (HostAddress)
import qualified Network.DNS as DNS

type Hash = Int

data Key = Key !Hash !ShortByteString deriving (Eq,Ord,Show)

data Value = Value UTCTime (Array Int IPv4) (IORef Int)

instance Show Value where
    show (Value t a _) = show t ++ ": " ++ show a

instance Eq Value where
    Value t1 _ _ == Value t2 _ _ = t1 == t2

instance Ord Value where
    Value t1 _ _ `compare` Value t2 _ _ = t1 `compare` t2

data DNSCacheConf = DNSCacheConf {
    dnsServers :: [HostName]
  , maxConcurrency :: Int
  , lifeTime :: NominalDiffTime
  }

type Lookup = Domain -> IO (Either DNS.DNSError IPv4)
type Wait = (Int -> Bool) -> IO ()

data DNSCache = DNSCache {
    cacheref :: IORef (PSQ Key Value)
  , limvar :: TVar Int
  , limit :: Int
  , life :: NominalDiffTime
  }
