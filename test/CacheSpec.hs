{-# LANGUAGE OverloadedStrings #-}

module CacheSpec where

import Test.Hspec
import Network.DNS hiding (lookup)
import Network.DNS.Cache
import Prelude hiding (lookup)

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
            addr = 2010691677
        resolve      cache dom `shouldReturn` Right (Resolved addr)
        resolveCache cache dom `shouldReturn` Just (Right (Hit addr))
        lookupCache  cache dom `shouldReturn` Just addr
        lookup       cache dom `shouldReturn` Just addr
    it "resolves domains and caches nagative" $ withDNSCache cacheConf $ \cache -> do
        let dom = "not-exist.com"
        resolve      cache dom `shouldReturn` Left UnexpectedRDATA
        resolveCache cache dom `shouldReturn` Just (Left UnexpectedRDATA)
        lookupCache  cache dom `shouldReturn` Nothing
        lookup       cache dom `shouldReturn` Nothing
