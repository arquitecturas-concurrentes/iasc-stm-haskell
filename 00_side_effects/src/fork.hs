module Fork where

import System.IO
import Control.Concurrent

main :: IO ()
main = do 
    tid <- forkIO (hPutStr stdout "Hola")
    hPutStr stdout " mundo\n"

