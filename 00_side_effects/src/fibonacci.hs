module FibThread 
where

import Control.Concurrent
import Control.Concurrent.MVar

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibThread :: Int -> MVar Int -> IO ()
fibThread n mvar = putMVar mvar (fib n)

main :: IO ()
main = do
  putStrLn "Calculando fibonacci..."
  fibResult <- newEmptyMVar
  forkIO (fibThread 30 fibResult)
  a <- takeMVar fibResult
  putStrLn ("fib result: " ++ show a)
