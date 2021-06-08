import System.IO


main :: IO()
main = do
  a <- getLine
  hPutStr stdout a 
  hPutStr stdout "\n"