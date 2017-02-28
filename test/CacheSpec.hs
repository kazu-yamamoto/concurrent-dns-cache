{-# LANGUAGE OverloadedStrings #-}

module CacheSpec where

import Control.Concurrent.Async (concurrently)
import Network.DNS hiding (lookup)
import Network.DNS.Cache
import Prelude hiding (lookup)
import Test.Hspec

cacheConf :: DNSCacheConf
cacheConf = DNSCacheConf {
    resolvConfs    = [
         defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }
       , defaultResolvConf { resolvInfo = RCHostName "8.8.4.4" }
       ]
  , maxConcurrency = 10
  , minTTL         = 60
  , maxTTL         = 300
  , negativeTTL    = 300
  }

spec :: Spec
spec = describe "withDnsCache" $ do
    it "resolves domains and caches addresses" $ withDNSCache cacheConf $ \cache -> do
        let dom = "www.example.com"
            addr = 584628317
        resolve      cache dom `shouldReturn` Right (Resolved addr)
        resolveCache cache dom `shouldReturn` Just (Right (Hit addr))
        lookupCache  cache dom `shouldReturn` Just addr
        lookup       cache dom `shouldReturn` Just addr
    it "resolves domains and caches nagative" $ withDNSCache cacheConf $ \cache -> do
        let dom = "not-exist.com"
            err = UnexpectedRDATA
        resolve      cache dom `shouldReturn` Left err
        resolveCache cache dom `shouldReturn` Just (Left err)
        lookupCache  cache dom `shouldReturn` Nothing
        lookup       cache dom `shouldReturn` Nothing
    it "resolves domains and caches nagative" $ withDNSCache cacheConf $ \cache -> do
        let dom = "non-exist.org"
            err = NameError
        resolve      cache dom `shouldReturn` Left err
        resolveCache cache dom `shouldReturn` Just (Left err)
        lookupCache  cache dom `shouldReturn` Nothing
        lookup       cache dom `shouldReturn` Nothing
    it "waits for another query for the same domain" $ withDNSCache cacheConf $ \cache -> do
        let dom = "www.example.com"
            addr = 584628317
        (res1,res2) <- concurrently (resolve cache dom) (resolve cache dom)
        [res1,res2] `shouldContain` [Right (Resolved addr),Right (Hit addr)]
    it "just returns IP address" $ withDNSCache cacheConf $ \cache -> do
        let dom = "192.0.2.1"
            addr = 16908480
        resolve      cache dom `shouldReturn` Right (Numeric addr)
