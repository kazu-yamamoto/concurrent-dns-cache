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
   loop :: Int -> UTCTime -> Lookup -> Wait -> IO ()
   loop n beg lkup wait = do
       when (n `mod` 1000 == 0) $ do
           cur <- getCurrentTime
           putStrLn $ show n ++ ": " ++ show (cur `diffUTCTime` beg)
       edom <- try BS.getLine
       case edom of
           Left (SomeException _) -> do
               wait (== 0)
               putStrLn "Done."
           Right dom -> do
               wait (< maxConn)
               void $ forkIO (lkup dom >>= p dom)
               loop (n+1) beg lkup wait
   p _   (Right _) = return ()
   p dom (Left  e) = do
       putStr $ show e ++ " "
       BS.putStrLn dom
