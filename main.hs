{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.DNS.Cache

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = withDNSCache conf loop
 where
   conf = DNSCacheConf ["8.8.8.8","8.8.4.4"] 200 300
   loop lkup wait = do
       dom <- BS.getLine
       if dom == "" then
           return ()
         else do
           wait
           void $ forkIO (lkup dom >>= print)
