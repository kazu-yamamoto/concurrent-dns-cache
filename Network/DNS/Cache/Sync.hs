{-# LANGUAGE BangPatterns #-}
module Network.DNS.Cache.Sync where

import Control.Applicative ((<$>))
import Control.Concurrent.STM

newtype ConcVar = ConcVar (TVar Int)

newConcVar :: IO ConcVar
newConcVar = ConcVar <$> newTVarIO 0

wait :: ConcVar -> (Int -> Bool) -> IO ()
wait (ConcVar var) cond = atomically $ do
    x <- readTVar var
    check (cond x)

waitIncrease :: ConcVar -> Int -> IO ()
waitIncrease (ConcVar var) lim = atomically $ do
    x <- readTVar var
    check (x < lim)
    let !x' = x + 1
    writeTVar var x'

decrease :: ConcVar -> IO ()
decrease (ConcVar var) = atomically $ modifyTVar' var (subtract 1)
