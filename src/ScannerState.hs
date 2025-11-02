module ScannerState (
  ScannerState(..),
  getLineNo,
  peek,
  peek2,
  advance,
  advanceIf,
  advanceWhile,
) where

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

advanceWhile :: (Char -> Bool) -> ScannerState -> (String, ScannerState)
advanceWhile predicate state = 
  case advanceIf predicate state of
    Nothing -> ("", state)
    Just (c, newState) ->
      let (suffix, finalState) = advanceWhile predicate newState
      in (c:suffix, finalState)

