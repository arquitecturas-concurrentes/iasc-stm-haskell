module OrElse where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
    

someFailingOp :: TVar a -> STM ()
someFailingOp a =
    v <- readTVar a
    if v < 49494949
        then retry
        else return ()

successOp :: TVar a -> STM()
successOp a =
    v <- readTVar a
    if v > 0
        then writeTVar a 33723
        else retry

anElseOperation :: TVar a -> TVar a -> STM () 
anElseOperation a b =
    OrElse (someFailingOp a) (successOp b)

main = do
    a <- atomically (newTVar 2)
    b <- atomically (newTVar 1)
    atomically (anElseOperation a b)