module Main (main) where

import Scanner (scan)
import System.IO (getContents)
import Types (ScanResult (..), Token)

printElements :: (Show a) => [a] -> IO ()
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
