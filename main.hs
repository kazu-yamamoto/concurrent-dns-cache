{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException(..))
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as BS
import Network.DNS.Cache
import Data.Time

maxConn :: Int
maxConn = 50

main :: IO ()
main = do
    beg <- getCurrentTime
    withDNSCache conf (loop 1 beg)
 where
   conf = DNSCacheConf ["8.8.8.8","8.8.4.4"] maxConn 180
   loop :: Int -> UTCTime -> DNSCache -> IO ()
   loop n beg cache = do
       when (n `mod` 1000 == 0) $ do
           cur <- getCurrentTime
           putStrLn $ show n ++ ": " ++ show (cur `diffUTCTime` beg)
       edom <- try BS.getLine
       case edom of
           Left (SomeException _) -> do
               wait cache (== 0)
               putStrLn "Done."
           Right dom -> do
               wait cache (< maxConn)
               void $ forkIO (lookupHostAddress cache dom >>= p dom)
               loop (n+1) beg cache
   p _   (Right _) = return ()
   p dom (Left  e) = do
       putStr $ show e ++ " "
       BS.putStrLn dom
