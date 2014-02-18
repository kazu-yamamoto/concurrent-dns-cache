module Network.DNS.Cache.Types where

import Data.Array (Array)
import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.PSQueue (PSQ)
import Data.Time.Clock (UTCTime)
import Network.BSD (HostName)
import Network.DNS (Domain)
import qualified Network.DNS as DNS
-- import Network.Socket (HostAddress)
import Data.IP

type Hash = Int

data Key = Key !Hash !ShortByteString deriving (Eq,Ord,Show)

data Value = Value UTCTime (Array Int IPv4) (IORef Int)

instance Show Value where
    show (Value t a _) = show t ++ ": " ++ show a

instance Eq Value where
    Value t1 _ _ == Value t2 _ _ = t1 == t2

instance Ord Value where
    Value t1 _ _ `compare` Value t2 _ _ = t1 `compare` t2

data DNSCache = DNSCache (IORef (PSQ Key Value))

data DNSCacheConf = DNSCacheConf {
    dnsServers :: [HostName]
  , maxConcurrency :: Int
  , lifeTime :: Int
  }

type Lookup = Domain -> IO (Either DNS.DNSError IPv4)
