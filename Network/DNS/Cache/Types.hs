module Network.DNS.Cache.Types (
    Key
  , Prio
  , Value(..)
  , Entry
  , Result(..)
  , TTL
  , HostAddress
  , Domain
  , DNSError(..)
  ) where

import Data.Array.Unboxed (UArray)
import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.Time (UTCTime)
import Network.DNS (Domain, DNSError(..))
import Network.Socket (HostAddress)

type Key = ShortByteString -- avoiding memory fragmentation
type Prio = UTCTime

-- fixme: if UArray causes memory fragments,
--        we should use real-time queue instaed.
data Value = Value (UArray Int HostAddress) (IORef Int)

instance Show Value where
    show (Value a _) = show a

type TTL = Int

-- | Information of positive result.
data Result =
  -- | An address obtained from the cache.
    Hit HostAddress
  -- | An address resolved from cache DNS servers.
  | Resolved HostAddress
  -- | Specified domain is IP address. So, it is converted into a numeric address.
  | Numeric HostAddress
  deriving (Eq,Show)

type Entry = Either DNSError Value
