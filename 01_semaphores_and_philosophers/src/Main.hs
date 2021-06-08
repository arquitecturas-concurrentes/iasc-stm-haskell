module Main where

import System.IO
import Control.Concurrent.STM

{- readTVar  :: TVar a -> STM a -}
{- writeTVar :: TVar a -> a -> STM () -}
{- atomically :: STM a -> IO a -}
type Counter = TVar Int

incTVar :: Counter -> STM ()
incTVar counter = do
    val <- readTVar counter
    writeTVar counter (val + 1)

-- badCall :: Counter -> IO ()
-- badCall cont = do
--     hPutStr stdout "Incrementando el contador en 1..."
--     incTVar cont

goodCall :: Counter -> IO()
goodCall cont = do
    hPutStr stdout "Incrementando el contador en 1..."
    atomically (incTVar cont)


main = do
    cont <- atomically (newTVar 3)
    goodCall cont 
    hPutStr stdout "\nTermine!\n"