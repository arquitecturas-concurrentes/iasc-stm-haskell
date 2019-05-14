module Main where

import System.IO
import Data.IORef

{-:t readIORef  -> readIORef :: IORef a -> IO a -}
{-:t writeIORef -> writeIORef :: IORef a -> a -> IO () -}
incRef :: IORef Int -> IO ()
incRef var = do
    val <- readIORef var
    writeIORef var (val+1)
                  
main = do
  v <- newIORef 1
  incRef v {-Incrementando el valor-} 
  val <- readIORef v
  hPutStr stdout (show val)