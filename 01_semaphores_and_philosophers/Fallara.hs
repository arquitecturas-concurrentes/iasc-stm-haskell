module Fallara where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

efectoIO :: IO()
efectoIO = hPutStr stdout "Hola efecto de lado!"

main = do
    a <- atomically (newTVar 2)
    b <- atomically (newTVar 1)
    atomically ( do
        x <- readTVar a
        y <- readTVar b
        if y<x then efectoIO
               else return())