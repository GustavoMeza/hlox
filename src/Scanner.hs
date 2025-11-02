module Scanner (scan) where

import Types
import ScannerState
import CharLib

scan :: String -> ScanResult
scan src = scanImpl (ScannerState src 0)

data ScanTokenResult
  = FoundToken Token ScannerState
  | Ignored ScannerState
  | End
  | ScanTokenError Int

scanImpl :: ScannerState -> ScanResult
scanImpl state = 
  case scanToken state of
    FoundToken token newState -> case scanImpl newState of
      TokenList tokens -> TokenList (token:tokens)
      ScanError lineNo -> ScanError lineNo
    Ignored newState         -> scanImpl newState
    End                      -> TokenList []
    ScanTokenError lineNo    -> ScanError lineNo

scanToken :: ScannerState -> ScanTokenResult
scanToken state =
  case advance state of
    Nothing                   -> End
    Just (c, newScannerState) -> scanTokenStartingWith c newScannerState

scanTokenStartingWith :: Char -> ScannerState -> ScanTokenResult
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

scanTokenWithMaybeEqual :: Token -> Token -> ScannerState -> ScanTokenResult
scanTokenWithMaybeEqual tokenIf tokenElse state =
  case advanceIf (== '=') state of
    Nothing -> FoundToken tokenElse state
    Just (_, newState) -> FoundToken tokenIf newState

scanSlashOrIgnoreComment :: ScannerState -> ScanTokenResult
scanSlashOrIgnoreComment state =
  case peek state of
    Just '/' -> Ignored (advanceUntilEndOfLine state)
    _  -> FoundToken Slash state

advanceUntilEndOfLine :: ScannerState -> ScannerState
advanceUntilEndOfLine state =
  let (_, newState) = advanceWhile (/= '\n') state
  in newState

scanString :: ScannerState -> ScanTokenResult
scanString state =
  let (str, newState) = advanceWhile (/= '"') state
  in case advanceIf (== '"') newState of
    Nothing -> ScanTokenError (getLineNo newState)
    Just (_, finalState) -> FoundToken (StringToken str) finalState
    
scanNumber :: Char -> ScannerState -> ScanTokenResult
scanNumber c state =
  let
    (restOfIntegerPart, stateAfterInteger) = advanceWhile isDigit state
    integerPart = c:restOfIntegerPart
    (numberStr, finalState) = tryScanFractionalPart integerPart stateAfterInteger
    number = read numberStr :: Double
  in FoundToken (Number number) finalState

tryScanFractionalPart :: String -> ScannerState -> (String, ScannerState)
tryScanFractionalPart integerPart state =
  case peek state of
    Just '.' -> case peekNext state of
      Just c | isDigit c ->
        let
          Just (_, stateAfterPoint) = advance state
          (fractionalPart, stateAfterFractional) = advanceWhile isDigit stateAfterPoint
          lexeme = integerPart ++ "." ++ fractionalPart
        in (lexeme, stateAfterFractional)
      _ -> (integerPart, state)
    _ -> (integerPart, state)

scanWord :: Char -> ScannerState -> ScanTokenResult
scanWord c state = 
  let
    (restOfWord, stateAfterWord) = advanceWhile isAlphaNumeric state
    word = c:restOfWord
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
