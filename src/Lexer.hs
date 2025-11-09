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
  | End
  deriving (Eq, Show)

data LexingError = LexingError Int deriving (Eq, Show)

lex :: String -> Either LexingError [Token]
lex src = do
  (tokens, _) <- lexImpl $ Scanner src 0
  return tokens

lexImpl :: Scanner -> Either LexingError ([Token], Scanner)
lexImpl scanner = do
  (token, scannerAfterToken) <- scanToken scanner
  if token == End
    then return ([End], scannerAfterToken)
    else do
      (restOfTokens, finalScanner) <- lexImpl scannerAfterToken
      return (token : restOfTokens, finalScanner)

type ScanTokenResult = Either LexingError (Token, Scanner)

scanToken :: Scanner -> ScanTokenResult
scanToken state =
  case advance state of
    Nothing -> Right (End, state)
    Just (c, newScanner) -> scanTokenStartingWith c newScanner

scanTokenStartingWith :: Char -> Scanner -> ScanTokenResult
scanTokenStartingWith c state =
  case c of
    '(' -> Right (LeftParen, state)
    ')' -> Right (RightParen, state)
    '{' -> Right (LeftBrace, state)
    '}' -> Right (RightBrace, state)
    ',' -> Right (Comma, state)
    '.' -> Right (Dot, state)
    '-' -> Right (Minus, state)
    '+' -> Right (Plus, state)
    ';' -> Right (Semicolon, state)
    '*' -> Right (Star, state)
    '!' -> Right $ scanTokenWithMaybeEqual BangEqual Bang state
    '=' -> Right $ scanTokenWithMaybeEqual EqualEqual Equal state
    '<' -> Right $ scanTokenWithMaybeEqual LessEqual Less state
    '>' -> Right $ scanTokenWithMaybeEqual GreaterEqual Greater state
    '/' -> scanSlashOrIgnoreComment state
    '"' -> scanString state
    c | isWhiteSpace c -> scanToken state
    c | isDigit c -> scanNumber c state
    c | isAlpha c -> Right $ scanWord c state
    _ -> Left $ LexingError $ getLineNo state

scanTokenWithMaybeEqual :: Token -> Token -> Scanner -> (Token, Scanner)
scanTokenWithMaybeEqual tokenIf tokenElse state =
  case advanceIf (== '=') state of
    Nothing -> (tokenElse, state)
    Just (_, newState) -> (tokenIf, newState)

scanSlashOrIgnoreComment :: Scanner -> ScanTokenResult
scanSlashOrIgnoreComment state =
  case peek state of
    Just '/' -> scanToken $ advanceUntilEndOfLine state
    _ -> Right (Slash, state)

advanceUntilEndOfLine :: Scanner -> Scanner
advanceUntilEndOfLine state =
  let (_, newState) = advanceWhile (/= '\n') state
   in newState

scanString :: Scanner -> ScanTokenResult
scanString state =
  let (str, newState) = advanceWhile (/= '"') state
   in case advanceIf (== '"') newState of
        Nothing -> Left $ LexingError $ getLineNo newState
        Just (_, finalState) -> Right (StringToken str, finalState)

scanNumber :: Char -> Scanner -> ScanTokenResult
scanNumber c state =
  let (restOfIntegerPart, stateAfterInteger) = advanceWhile isDigit state
      integerPart = c : restOfIntegerPart
      (numberStr, finalState) = tryScanFractionalPart integerPart stateAfterInteger
      maybeNumber = readMaybe numberStr :: Maybe Double
   in case maybeNumber of
        Nothing -> Left $ LexingError $ getLineNo finalState
        Just number -> Right (Number number, finalState)

tryScanFractionalPart :: String -> Scanner -> (String, Scanner)
tryScanFractionalPart integerPart state =
  case peek state of
    Just '.' -> case peekNext state of
      Just c
        | isDigit c ->
            let Just (_, stateAfterPoint) = advance state
                (fractionalPart, stateAfterFractional) = advanceWhile isDigit stateAfterPoint
                lexeme = integerPart ++ "." ++ fractionalPart
             in (lexeme, stateAfterFractional)
      _ -> (integerPart, state)
    _ -> (integerPart, state)

scanWord :: Char -> Scanner -> (Token, Scanner)
scanWord c state =
  let (restOfWord, stateAfterWord) = advanceWhile isAlphaNumeric state
      word = c : restOfWord
      token = wordToToken word
   in (token, stateAfterWord)

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
