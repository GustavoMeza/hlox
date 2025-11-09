module Lexer (Lexer.lex, LexingError (..), Token (..)) where

import CharLib
import Scanner
import Text.Read (readMaybe)

data Token
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Slash
  | StringToken String
  | Number Double
  | AndToken
  | ClassToken
  | ElseToken
  | FalseToken
  | ForToken
  | FunToken
  | IfToken
  | NilToken
  | OrToken
  | PrintToken
  | ReturnToken
  | SuperToken
  | ThisToken
  | TrueToken
  | VarToken
  | WhileToken
  | Identifier String
  deriving (Eq, Show)

data LexingError = LexingError Int deriving (Eq, Show)

lex :: String -> Either LexingError [Token]
lex src = do
  (tokens, _) <- extractTokens $ Scanner src 0
  return tokens

type ScanTokenResult = Either LexingError ([Token], Scanner)

extractTokens :: Scanner -> ScanTokenResult
extractTokens scanner = do
  case advance scanner of
    Nothing -> return ([], scanner)
    Just (c, newScanner) -> scanTokenStartingWith c newScanner

scanTokenStartingWith :: Char -> Scanner -> ScanTokenResult
scanTokenStartingWith c scanner =
  case c of
    '(' -> prependToken LeftParen scanner
    ')' -> prependToken RightParen scanner
    '{' -> prependToken LeftBrace scanner
    '}' -> prependToken RightBrace scanner
    ',' -> prependToken Comma scanner
    '.' -> prependToken Dot scanner
    '-' -> prependToken Minus scanner
    '+' -> prependToken Plus scanner
    ';' -> prependToken Semicolon scanner
    '*' -> prependToken Star scanner
    '!' -> scanTokenWithMaybeEqual BangEqual Bang scanner
    '=' -> scanTokenWithMaybeEqual EqualEqual Equal scanner
    '<' -> scanTokenWithMaybeEqual LessEqual Less scanner
    '>' -> scanTokenWithMaybeEqual GreaterEqual Greater scanner
    '/' -> scanSlashOrIgnoreComment scanner
    '"' -> scanString scanner
    c | isWhiteSpace c -> extractTokens scanner
    c | isDigit c -> scanNumber c scanner
    c | isAlpha c -> scanWord c scanner
    _ -> Left $ LexingError $ getLineNo scanner

scanTokenWithMaybeEqual :: Token -> Token -> Scanner -> ScanTokenResult
scanTokenWithMaybeEqual tokenIf tokenElse scanner =
  case advanceIf (== '=') scanner of
    Nothing -> prependToken tokenElse scanner
    Just (_, newScanner) -> prependToken tokenIf newScanner

scanSlashOrIgnoreComment :: Scanner -> ScanTokenResult
scanSlashOrIgnoreComment scanner =
  case peek scanner of
    Just '/' -> extractTokens $ advanceUntilEndOfLine scanner
    _ -> prependToken Slash scanner

scanString :: Scanner -> ScanTokenResult
scanString scanner =
  let (str, newScanner) = advanceWhile (/= '"') scanner
   in case advanceIf (== '"') newScanner of
        Nothing -> Left $ LexingError $ getLineNo newScanner
        Just (_, finalScanner) -> prependToken (StringToken str) finalScanner

scanNumber :: Char -> Scanner -> ScanTokenResult
scanNumber c scanner =
  let (restOfIntegerPart, scannerAfterInteger) = advanceWhile isDigit scanner
      integerPart = c : restOfIntegerPart
      (fractionalPart, finalScanner) = advanceIfFractionalPart scannerAfterInteger
      lexeme = integerPart ++ fractionalPart
      maybeNumber = readMaybe lexeme :: Maybe Double
   in case maybeNumber of
        Nothing -> Left $ LexingError $ getLineNo finalScanner
        Just number -> prependToken (Number number) finalScanner

scanWord :: Char -> Scanner -> ScanTokenResult
scanWord c scanner =
  let (restOfWord, scannerAfterWord) = advanceWhile isAlphaNumeric scanner
      word = c : restOfWord
      token = wordToToken word
   in prependToken token scannerAfterWord

wordToToken :: String -> Token
wordToToken word =
  case word of
    "and" -> AndToken
    "class" -> ClassToken
    "else" -> ElseToken
    "false" -> FalseToken
    "for" -> ForToken
    "fun" -> FunToken
    "if" -> IfToken
    "nil" -> NilToken
    "or" -> OrToken
    "print" -> PrintToken
    "return" -> ReturnToken
    "super" -> SuperToken
    "this" -> ThisToken
    "true" -> TrueToken
    "var" -> VarToken
    "while" -> WhileToken
    _ -> Identifier word

prependToken :: Token -> Scanner -> ScanTokenResult
prependToken token scanner = do
  (restOfTokens, finalScanner) <- extractTokens scanner
  return (token : restOfTokens, finalScanner)

advanceUntilEndOfLine :: Scanner -> Scanner
advanceUntilEndOfLine scanner =
  let (_, newScanner) = advanceWhile (/= '\n') scanner
   in newScanner

advanceIfFractionalPart :: Scanner -> (String, Scanner)
advanceIfFractionalPart scanner =
  case peek scanner of
    Just '.' -> case peekNext scanner of
      Just c
        | isDigit c ->
            let Just (_, scannerAfterPoint) = advance scanner
                (fractionalPart, scannerAfterFractional) = advanceWhile isDigit scannerAfterPoint
             in ('.' : fractionalPart, scannerAfterFractional)
      _ -> ("", scanner)
    _ -> ("", scanner)
