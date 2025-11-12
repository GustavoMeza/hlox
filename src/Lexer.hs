module Lexer (Lexer.lex, LexingError (..), Token (..)) where

import CharLib
import Data.List
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

lex :: String -> Either [LexingError] [Token]
lex src =
  let stateAfterSrc = foldl' advanceLexing (ScanNewToken $ GlobalState [] [] 0) src
      (errors, tokens) = finalize stateAfterSrc
   in if null errors
        then Right tokens
        else Left errors

finalize :: ScanningState -> ([LexingError], [Token])
finalize (ScanNewToken state) = errorsAndTokens state
finalize (ScanMaybeEqual state token tokenEqual) = errorsAndTokens $ addToken token state
finalize (ScanMaybeComment state) = errorsAndTokens $ addToken Slash state
finalize (IgnoreUntilEol state) = errorsAndTokens state
finalize (ScanString state str) = errorsAndTokens $ addError state
finalize (ScanIntegerPart state str) = errorsAndTokens $ addNumberOrError str state
finalize (MaybeScanFractionalPart state str) = errorsAndTokens $ addToken Dot $ addNumberOrError str state
finalize (ScanFractionalPart state str) = errorsAndTokens $ addNumberOrError str state
finalize (ScanWord state str) = errorsAndTokens $ addToken (wordToToken str) state

data GlobalState = GlobalState [Token] [LexingError] Int

data ScanningState
  = ScanNewToken GlobalState
  | ScanMaybeEqual GlobalState Token Token
  | ScanMaybeComment GlobalState
  | IgnoreUntilEol GlobalState
  | ScanString GlobalState String
  | ScanIntegerPart GlobalState String
  | MaybeScanFractionalPart GlobalState String
  | ScanFractionalPart GlobalState String
  | ScanWord GlobalState String

advanceLexing :: ScanningState -> Char -> ScanningState
advanceLexing (ScanNewToken state) c =
  case c of
    '(' -> ScanNewToken $ addToken LeftParen state
    ')' -> ScanNewToken $ addToken RightParen state
    '{' -> ScanNewToken $ addToken LeftBrace state
    '}' -> ScanNewToken $ addToken RightBrace state
    ',' -> ScanNewToken $ addToken Comma state
    '.' -> ScanNewToken $ addToken Dot state
    '-' -> ScanNewToken $ addToken Minus state
    '+' -> ScanNewToken $ addToken Plus state
    ';' -> ScanNewToken $ addToken Semicolon state
    '*' -> ScanNewToken $ addToken Star state
    '!' -> ScanMaybeEqual state Bang BangEqual
    '=' -> ScanMaybeEqual state Equal EqualEqual
    '<' -> ScanMaybeEqual state Less LessEqual
    '>' -> ScanMaybeEqual state Greater GreaterEqual
    '/' -> ScanMaybeComment state
    '"' -> ScanString state ""
    '\n' -> ScanNewToken $ incrementLine state
    c | isWhiteSpace c -> ScanNewToken state
    c | isDigit c -> ScanIntegerPart state [c]
    c | isAlpha c -> ScanWord state [c]
    _ -> ScanNewToken $ addError state
advanceLexing (ScanMaybeEqual state token tokenEqual) c =
  if c == '='
    then ScanNewToken $ addToken tokenEqual state
    else advanceLexing (ScanNewToken $ addToken token state) c
advanceLexing (ScanMaybeComment state) c =
  if c == '/'
    then IgnoreUntilEol state
    else advanceLexing (ScanNewToken $ addToken Slash state) c
advanceLexing (IgnoreUntilEol state) c =
  if c == '\n'
    then ScanNewToken $ incrementLine state
    else IgnoreUntilEol state
advanceLexing (ScanString state str) c =
  case c of
    '"' -> ScanNewToken $ addToken (StringToken $ reverse str) state
    '\n' -> ScanString (incrementLine state) (c : str)
    _ -> ScanString state (c : str)
advanceLexing (ScanIntegerPart state str) c =
  case c of
    c | isDigit c -> ScanIntegerPart state (c : str)
    '.' -> MaybeScanFractionalPart state str
    _ -> advanceLexing (ScanNewToken $ addNumberOrError str state) c
advanceLexing (MaybeScanFractionalPart state str) c =
  if isDigit c
    then ScanFractionalPart state (c : '.' : str)
    else advanceLexing (ScanNewToken $ addToken Dot $ addNumberOrError str state) c
advanceLexing (ScanFractionalPart state str) c =
  if isDigit c
    then ScanFractionalPart state (c : str)
    else advanceLexing (ScanNewToken $ addNumberOrError str state) c
advanceLexing (ScanWord state str) c =
  if isAlphaNumeric c
    then ScanWord state (c : str)
    else advanceLexing (ScanNewToken $ addToken (wordToToken str) state) c

wordToToken :: String -> Token
wordToToken reversedWord =
  let word = reverse reversedWord
   in case word of
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

addToken :: Token -> GlobalState -> GlobalState
addToken token (GlobalState tokens errors lineNo) =
  GlobalState (token : tokens) errors lineNo

addNumberOrError :: String -> GlobalState -> GlobalState
addNumberOrError str state =
  let maybeNumber = readMaybe (reverse str) :: Maybe Double
   in case maybeNumber of
        Nothing -> addError state
        Just number -> addToken (Number number) state

incrementLine :: GlobalState -> GlobalState
incrementLine (GlobalState tokens errors lineNo) =
  GlobalState tokens errors (lineNo + 1)

addError :: GlobalState -> GlobalState
addError (GlobalState tokens errors lineNo) =
  GlobalState tokens (LexingError lineNo : errors) lineNo

errorsAndTokens :: GlobalState -> ([LexingError], [Token])
errorsAndTokens (GlobalState tokens errors lineNo) =
  (reverse errors, reverse tokens)
