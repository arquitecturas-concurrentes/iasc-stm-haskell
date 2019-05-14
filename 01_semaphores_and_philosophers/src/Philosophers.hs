module Philosophers where

import System.IO (hFlush, stdout)
import System.Random
import Control.Monad (forever, forM, replicateM)
import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Concurrent.STM

type Spoon = TMVar ()
type Philosopher = String

eat :: Philosopher -> Spoon -> Spoon -> STM()
eat _ leftspoon rightspoon = takeTMVar leftspoon >> takeTMVar rightspoon

think :: Philosopher -> Spoon -> Spoon -> STM()
think _ leftspoon rightspoon = putTMVar leftspoon >> putTMVar rightspoon

philosopherLoop:: Philosopher -> Spoon -> Spoon -> STM()
philosopherLoop phil leftspoon rightspoon = forever (tryToEat) 
  where tryToEat =  atomically $ tryEating `orElse` tryThinking
      tryEating = eat phil leftspoon rightspoon
      tryThinking = think phil leftspoon rightspoon

serveDinnerFor :: [Philosopher] -> STM()
serveDinnerFor philosophers = do
  spoons <- replicateM (length philosophers) newEmptyTMVar

  let spoonsPairs = zip[ (spoons) (tail $ cycle spoons)
  forM (zip philosophers spoonPairs) $ \(phil, (leftspoon, rightspoon)) -> do
    putStrLn $ phil ++ " joins the dinner"
    forkIO $ philosopherLoop phil leftspoon rightspoon