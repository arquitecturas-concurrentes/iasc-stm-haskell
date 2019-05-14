module PhilosophersTChan where

import System.IO (hFlush, stdout)
import System.Random
import Control.Monad (forever, forM, replicateM)
import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Concurrent.STM

type Logger = TChan String

type Spoon = TMVar ()
type Philosopher = String

eat :: Philosopher -> Spoon -> Spoon -> STM ()
eat _ ls rs = takeTMVar ls >> takeTMVar rs

think :: Philosopher -> Spoon -> Spoon -> STM ()
think _ ls rs = putTMVar ls () >> putTMVar rs ()

philosopherLoop :: Philosopher -> Spoon -> Spoon -> Logger -> IO ()
philosopherLoop phil leftspoon rightpoon log = forever (doSomething >> randomThreadDelay)
    where doSomething = atomically $ tryEating `orElse` tryThinking
        tryEating   = eat   phil leftspoon rightpoon >> (writeTChan log $ phil ++ " eats")
        tryThinking = think phil leftspoon rightpoon >> (writeTChan log $ phil ++ " Thinks")


philosophersDinner :: [Philosopher] -> IO ()
philosophersDinner philsophers = do
    log <- newTChanIO

    spoons <- replicateM (length philsophers) newEmptyTMVarIO
    let spoonPairs = zip (spoons) (tail $ cycle spoons)
    forM (zip philsophers spoonPairs) $ \(phil, (ls, rs)) -> do
        forkIO $ philosopherLoop phil ls rs log
    forever $ printOutputLog log

printOutputLog log = atomically (readTChan log) >>= putStrLn >> hFlush stdout

randomThreadDelay = do
    seconds <- getStdRandom $ randomR (1,9)
    threadDelay $ seconds * 100000