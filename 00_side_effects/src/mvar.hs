module MVarExample
where

import Control.Concurrent
import Control.Concurrent.MVar


threadA :: MVar Int -> MVar Float -> IO ()
threadA valueAMVar valueBMVar = do
  putMVar valueAMVar 5
  v <- takeMVar valueBMVar
  putStrLn (show v)


threadB :: MVar Int -> MVar Float -> IO ()
threadB valueAMVar valueBMVar = do
  x <- takeMVar valueAMVar
  putStrLn (show x)
  putMVar valueBMVar (fromIntegral x * 1.5)

main :: IO ()
main = do
  aMVar <- newEmptyMVar 
  bMVar <- newEmptyMVar
  forkIO ( threadA aMVar bMVar )
  forkIO ( threadB aMVar bMVar )
  threadDelay 3000 -- wait for threadA and threadB to finish (sleazy)