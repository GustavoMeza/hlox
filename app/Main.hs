module Main (main) where

import Parser
import Lexer
import System.IO (getContents)

main :: IO ()
main = do
  sourceCode <- getContents
  let lexingResult = Lexer.lex sourceCode
   in case lexingResult of
        Left error -> putStrLn $ show error
        Right tokens -> case parseExpr (ParsingState tokens) of
          Left error -> putStrLn $ show error
          Right (_, expr) -> putStrLn $ show expr
