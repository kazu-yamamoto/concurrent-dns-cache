module Network.DNS.Cache.Utils where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import Network.DNS.Cache.Types

isIPAddr :: Domain -> Bool
isIPAddr hn = length groups == 4 && all ip groups
  where
    groups = BS.split '.' hn
    ip x = BS.length x <= 3
        && BS.all isDigit x
        && read (BS.unpack x) <= (255 :: Int)

fromResult :: Result -> HostAddress
fromResult (Hit      addr) = addr
fromResult (Resolved addr) = addr
fromResult (Numeric  addr) = addr

fromEither :: Either DNSError Result -> Maybe HostAddress
fromEither (Right res) = Just (fromResult res)
fromEither (Left    _) = Nothing
