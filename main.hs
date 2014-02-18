{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException(..))
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Network.DNS.Cache

maxConn :: Int
maxConn = 200

main :: IO ()
main = withDNSCache conf loop
 where
   conf = DNSCacheConf ["8.8.8.8","8.8.4.4"] maxConn 180
   loop :: Lookup -> Wait -> IO ()
   loop lkup wait = do
       edom <- try BS.getLine
       case edom of
           Left (SomeException _) -> do
               wait (== 0)
               putStrLn "Done."
           Right dom -> do
               wait (< maxConn)
               void $ forkIO (lkup dom >>= p dom)
               loop lkup wait
   p dom r = do
       putStr $ show r ++ " "
       BS.putStrLn dom
