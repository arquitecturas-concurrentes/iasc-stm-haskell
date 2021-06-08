module Fork where

import System.IO
import Control.Concurrent

-- Introducing MVars
-- data MVar a  -- abstract

-- newEmptyMVar :: IO (MVar a)
-- newMVar      :: a -> IO (MVar a)
-- takeMVar     :: MVar a -> IO a
-- putMVar      :: MVar a -> a -> IO ()

main :: IO ()
main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'X'; putMVar m 'Y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r