{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Network.DNS (Domain)
import Network.DNS.Cache
import Control.Exception (try, SomeException(..))

maxConn :: Int
maxConn = 200

main :: IO ()
main = withDNSCache conf loop
 where
   conf = DNSCacheConf ["8.8.8.8","8.8.4.4"] maxConn 300
   loop :: Lookup -> Wait -> IO ()
   loop lkup wait = do
       edom <- try BS.getLine
       case edom of
           Left (SomeException _) -> do
               wait (== 0)
               putStrLn "Done."
           Right dom
             | sanityCheck dom -> do
                 wait (< maxConn)
                 void $ forkIO (lkup dom >>= print)
                 loop lkup wait
             | otherwise -> loop lkup wait

sanityCheck :: Domain -> Bool
sanityCheck "" = False
sanityCheck dom
  | ':' `BS.elem` dom           = False
  | '/' `BS.elem` dom           = False
  | BS.length dom > 253         = False
  | any (\x -> BS.length x > 63) (BS.split '.' dom) = False
  | isIPAddr dom                = False
sanityCheck _ = True

isIPAddr :: Domain -> Bool
isIPAddr hn = length groups == 4 && all ip groups
  where
    groups = BS.split '.' hn
    ip x = BS.length x <= 3
        && BS.all (\ e -> e >= '0' && e <= '9') x
        && read (BS.unpack x) <= (255 :: Int)
