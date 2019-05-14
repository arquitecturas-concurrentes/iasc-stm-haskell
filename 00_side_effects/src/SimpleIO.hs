module SimpleIO where

import System.IO

preprend :: Handle -> IO String
prepend h = 
    s <- hGetLine h
    hPutStr h ("Linea de prepend antes de la linea leida del handle.")
    return s

main = do
    handle <- openFile "./hola.txt" ReadWriteMode
    str <- prepend handle
    hClose handle
    hPutStr stdout "Hola mundo!"