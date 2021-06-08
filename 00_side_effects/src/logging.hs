module MVarLogging where

import System.IO
import Control.Concurrent

data Logger = Logger ( MVar Command )
data Command = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let log = Logger m
  forkIO (runLog log)
  return log

logMessage :: Logger -> String -> IO ()
logMessage (Logger l) str = putMVar l (Message str)

logStop :: Logger -> IO ()
logStop (Logger l) = do
  m <- newEmptyMVar
  putMVar l (Stop m)
  takeMVar m

-- loop to run...  
runLog :: Logger -> IO ()
runLog (Logger l) = loop
  where 
    loop = do
      cmd <- takeMVar l
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "Logger: Stop..."
          putMVar s ()



main :: IO ()
main = do
  l <- initLogger
  logMessage l "Hola Mundo!!"
  logMessage l "Blah"
  logMessage l "Adios"
  logStop l