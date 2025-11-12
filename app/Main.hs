module Main (main) where

import Lexer
import System.IO (getContents)

printElements :: Show a => [a] -> IO ()
printElements arr = mapM_ (putStrLn . show) arr

main :: IO ()
main = do
  sourceCode <- getContents
  let lexingResult = Lexer.lex sourceCode
   in case lexingResult of
        Left error -> putStrLn $ show error
        Right tokens -> printElements tokens
