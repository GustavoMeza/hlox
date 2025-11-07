module Scanner
  ( Scanner (..),
    getLineNo,
    peek,
    peekNext,
    advance,
    advanceIf,
    advanceWhile,
  )
where

data Scanner = Scanner String Int

getLineNo :: Scanner -> Int
getLineNo (Scanner _ lineNo) = lineNo

getSrc :: Scanner -> String
getSrc (Scanner src _) = src

peek :: Scanner -> Maybe Char
peek state =
  case getSrc state of
    "" -> Nothing
    c : _ -> Just c

peekNext :: Scanner -> Maybe Char
peekNext state =
  case getSrc state of
    _ : (c2 : _) -> Just c2
    _ -> Nothing

advance :: Scanner -> Maybe (Char, Scanner)
advance (Scanner src lineNo) =
  case src of
    "" -> Nothing
    c : rest ->
      let newLineNo = if c == '\n' then lineNo + 1 else lineNo
       in Just (c, Scanner rest newLineNo)

advanceIf :: (Char -> Bool) -> Scanner -> Maybe (Char, Scanner)
advanceIf predicate state =
  case peek state of
    Just c | predicate c -> advance state
    _ -> Nothing

advanceWhile :: (Char -> Bool) -> Scanner -> (String, Scanner)
advanceWhile predicate state =
  case advanceIf predicate state of
    Nothing -> ("", state)
    Just (c, newState) ->
      let (suffix, finalState) = advanceWhile predicate newState
       in (c : suffix, finalState)
