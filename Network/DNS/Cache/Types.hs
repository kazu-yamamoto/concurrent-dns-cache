module Network.DNS.Cache.Types where

import Data.Array (Array)
import Data.ByteString.Short (ShortByteString)
import Data.IORef (IORef)
import Data.IP
import Data.Time (UTCTime)

type Hash = Int

data Key = Key !Hash !ShortByteString deriving (Eq,Ord,Show)

type Prio = UTCTime

data Value = Value (Array Int IPv4) (IORef Int)

instance Show Value where
    show (Value a _) = show a
