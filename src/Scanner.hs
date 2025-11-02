module Scanner (scan) where

import Types (Token(..), ScanResult(..))

scan :: String -> ScanResult
scan src = scanImpl (ScannerState src 0)

scanImpl :: ScannerState -> ScanResult
scanImpl state = 
  case scanToken state of
    FoundToken token newState -> case scanImpl newState of
      TokenList tokens -> TokenList (token:tokens)
      error -> error
    Ignored newState         -> scanImpl newState
    End                      -> TokenList []
    ScanTokenError lineNo    -> ScanError lineNo

data ScannerState = ScannerState String Int

getLineNo :: ScannerState -> Int
getLineNo (ScannerState _ lineNo) = lineNo

getSrc :: ScannerState -> String
getSrc (ScannerState src _) = src

peek :: ScannerState -> Maybe Char
peek state = 
  case getSrc state of
    "" -> Nothing
    c:_ -> Just c

peek2 :: ScannerState -> Maybe (Char,Char)
peek2 state = 
  case getSrc state of
    "" -> Nothing
    c:"" -> Nothing
    c1:(c2:_) -> Just (c1,c2)

advance :: ScannerState -> Maybe (Char, ScannerState)
advance (ScannerState src lineNo) = 
  case src of
    "" -> Nothing
    c:rest -> let newLineNo = if c == '\n' then lineNo+1 else lineNo
              in Just (c, ScannerState rest newLineNo)

advanceIf :: (Char -> Bool) -> ScannerState -> Maybe (Char, ScannerState)
advanceIf predicate state =
  case peek state of
    Just c | predicate c -> advance state
    _ -> Nothing

data ScanTokenResult
  = FoundToken Token ScannerState
  | Ignored ScannerState
  | End
  | ScanTokenError Int

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
  case advanceIf (\c -> c == '=') state of
    Nothing -> FoundToken tokenElse state
    Just (_, newState) -> FoundToken tokenIf newState

scanSlashOrIgnoreComment :: ScannerState -> ScanTokenResult
scanSlashOrIgnoreComment state =
  case peek state of
    Just '/' -> Ignored (advanceUntilEndOfLine state)
    _  -> FoundToken Slash state

advanceUntilEndOfLine :: ScannerState -> ScannerState
advanceUntilEndOfLine state =
  let (_, newState) = advanceWhile (\c -> c /= '\n') state
  in newState

advanceWhile :: (Char -> Bool) -> ScannerState -> (String, ScannerState)
advanceWhile predicate state = 
  case advanceIf predicate state of
    Nothing -> ("", state)
    Just (c, newState) ->
      let (suffix, finalState) = advanceWhile predicate newState
      in (c:suffix, finalState)

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \r\t\n"

scanString :: ScannerState -> ScanTokenResult
scanString state =
  let (str, newState) = advanceWhile (\c -> c /= '"') state
  in case advanceIf (\c -> c == '"') newState of
    Nothing -> ScanTokenError (getLineNo newState)
    Just (_, finalState) -> FoundToken (StringToken str) finalState
    
scanNumber :: Char -> ScannerState -> ScanTokenResult
scanNumber c state =
  let
    (restOfIntegerPart, stateAfterInteger) = advanceWhile isDigit state
    integerPart = c:restOfIntegerPart
    (numberStr, finalState) = case peek2 stateAfterInteger of
      Just ('.',c) | isDigit c -> 
        case advance stateAfterInteger of
          Nothing -> (integerPart, stateAfterInteger)
          Just (_, stateAfterPoint) -> 
            let (fractionalPart, stateAfterFractional) = advanceWhile isDigit stateAfterPoint
            in (integerPart ++ "." ++ fractionalPart, stateAfterFractional)
      _ -> (integerPart, stateAfterInteger)
    number = read numberStr :: Double
  in FoundToken (Number number) finalState

isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

scanWord :: Char -> ScannerState -> ScanTokenResult
scanWord c state = 
  let
    (restOfWord, stateAfterWord) = advanceWhile isAlphaNumeric state
    word = c:restOfWord
  in FoundToken (wordToToken word) stateAfterWord

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

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isDigit c || isAlpha c
