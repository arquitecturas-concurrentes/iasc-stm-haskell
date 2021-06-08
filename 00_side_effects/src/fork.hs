module Fork where

import System.IO
import Control.Concurrent

main :: IO ()
main = do 
    forkIO ( hPutStr stdout "Hola" )
    hPutStr stdout " mundo\n"

