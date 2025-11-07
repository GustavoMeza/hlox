module Lexer (Token (..), LexingResult (..), Lexer.lex) where

import CharLib
import Scanner

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

data LexingResult
  = TokenList [Token]
  | LexingError Int
  deriving (Eq, Show)

lex :: String -> LexingResult
lex src = scanImpl (Scanner src 0)

scanImpl :: Scanner -> LexingResult
scanImpl state =
  case scanToken state of
    FoundToken token newState -> case scanImpl newState of
      TokenList tokens -> TokenList (token : tokens)
      LexingError lineNo -> LexingError lineNo
    Ignored newState -> scanImpl newState
    End -> TokenList []
    ScanTokenError lineNo -> LexingError lineNo

data ScanTokenResult
  = FoundToken Token Scanner
  | Ignored Scanner
  | End
  | ScanTokenError Int

scanToken :: Scanner -> ScanTokenResult
scanToken state =
  case advance state of
    Nothing -> End
    Just (c, newScanner) -> scanTokenStartingWith c newScanner

scanTokenStartingWith :: Char -> Scanner -> ScanTokenResult
scanTokenStartingWith c state =
  case c of
    '(' -> FoundToken LeftParen state
    ')' -> FoundToken RightParen state
    '{' -> FoundToken LeftBrace state
    '}' -> FoundToken RightBrace state
    ',' -> FoundToken Comma state
    '.' -> FoundToken Dot state
    '-' -> FoundToken Minus state
    '+' -> FoundToken Plus state
    ';' -> FoundToken Semicolon state
    '*' -> FoundToken Star state
    '!' -> scanTokenWithMaybeEqual BangEqual Bang state
    '=' -> scanTokenWithMaybeEqual EqualEqual Equal state
    '<' -> scanTokenWithMaybeEqual LessEqual Less state
    '>' -> scanTokenWithMaybeEqual GreaterEqual Greater state
    '/' -> scanSlashOrIgnoreComment state
    '"' -> scanString state
    c | isWhiteSpace c -> Ignored state
    c | isDigit c -> scanNumber c state
    c | isAlpha c -> scanWord c state
    _ -> ScanTokenError (getLineNo state)

scanTokenWithMaybeEqual :: Token -> Token -> Scanner -> ScanTokenResult
scanTokenWithMaybeEqual tokenIf tokenElse state =
  case advanceIf (== '=') state of
    Nothing -> FoundToken tokenElse state
    Just (_, newState) -> FoundToken tokenIf newState

scanSlashOrIgnoreComment :: Scanner -> ScanTokenResult
scanSlashOrIgnoreComment state =
  case peek state of
    Just '/' -> Ignored (advanceUntilEndOfLine state)
    _ -> FoundToken Slash state

advanceUntilEndOfLine :: Scanner -> Scanner
advanceUntilEndOfLine state =
  let (_, newState) = advanceWhile (/= '\n') state
   in newState

scanString :: Scanner -> ScanTokenResult
scanString state =
  let (str, newState) = advanceWhile (/= '"') state
   in case advanceIf (== '"') newState of
        Nothing -> ScanTokenError (getLineNo newState)
        Just (_, finalState) -> FoundToken (StringToken str) finalState

scanNumber :: Char -> Scanner -> ScanTokenResult
scanNumber c state =
  let (restOfIntegerPart, stateAfterInteger) = advanceWhile isDigit state
      integerPart = c : restOfIntegerPart
      (numberStr, finalState) = tryScanFractionalPart integerPart stateAfterInteger
      number = read numberStr :: Double
   in FoundToken (Number number) finalState

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

scanWord :: Char -> Scanner -> ScanTokenResult
scanWord c state =
  let (restOfWord, stateAfterWord) = advanceWhile isAlphaNumeric state
      word = c : restOfWord
      token = wordToToken word
   in FoundToken token stateAfterWord

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
