module Semaphore where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem aval = newTVarIO aval

p :: Semaphore -> STM()
p sem = do
    b <- readTVar sem
    if b
        then writeTVar sem False
        else retry

v :: Semaphore -> STM()
v sem = writeTVar sem True

