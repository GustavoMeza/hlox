module Main (main) where

import System.IO (getContents)
import Scanner (scan)
import Types (Token, ScanResult(..))

printElements :: Show a => [a] -> IO ()
printElements arr = mapM_ (putStrLn . show) arr

main :: IO ()
main = do
  sourceCode <- getContents
  let scanResult = scan sourceCode
  case scanResult of
    TokenList tokens -> do
      putStrLn "Scanning Succeeded. Printing Tokens."
      printElements tokens
    ScanError lineNo -> do
      putStrLn ("Scanner failed on line " ++ show lineNo)
      fail "Scanner error encountered."
