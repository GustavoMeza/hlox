module Main (main) where

import Lexer
import System.IO (getContents)

printElements :: (Show a) => [a] -> IO ()
printElements arr = mapM_ (putStrLn . show) arr

main :: IO ()
main = do
  sourceCode <- getContents
  let lexingResult = Lexer.lex sourceCode
  case lexingResult of
    TokenList tokens -> do
      putStrLn "Scanning Succeeded. Printing Tokens."
      printElements tokens
    LexingError lineNo -> do
      putStrLn ("Scanner failed on line " ++ show lineNo)
      fail "Scanner error encountered."
