module Network.DNS.Cache.Types (
    Hash
  , Key(..)
  , Prio
  , Value(..)
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

type Hash = Int

data Key = Key !Hash !ShortByteString deriving (Eq,Ord,Show)

type Prio = UTCTime

-- fixme: if UArray causes memory fragments,
--        we should use real-time queue instaed.
data Value = Value (UArray Int HostAddress) (IORef Int)

instance Show Value where
    show (Value a _) = show a

type TTL = Int

data Result = Hit HostAddress
            | Resolved HostAddress
            | Numeric HostAddress
            deriving Show
