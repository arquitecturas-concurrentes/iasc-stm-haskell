module Account 
where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Int

check :: Bool -> STM ()
check True  = return ()
check False = retry


{- readTVar  :: TVar a -> STM a -}
{- writeTVar :: TVar a -> a -> STM () -}
{- atomically :: STM a -> IO a -}

deposit :: Account -> Int -> STM ()
deposit account money = ...

withdraw :: Account -> Int -> STM ()
withdraw account money = ...


main = do
    a <- atomically (newTVar 200)
    b <- atomically (newTVar 100)
    atomically ( do {
      withdraw a 100
      deposit b 100
    })
    
    