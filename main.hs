module Main where

import Network.DNS.Cache

import Control.Concurrent
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Network.DNS.Cache.Types

maxConc :: Int
maxConc = 10

main :: IO ()
main = do
    let conf = DNSCacheConf ["8.8.8.8","8.8.4.4"] 10 300
    withDNSCache conf $ \lkup -> do
        doms <- BS.lines <$> BS.getContents
        mapM_ (\dom -> forkIO (lkup dom >>= print)) doms
        threadDelay 30000000

