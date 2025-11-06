module CharLib
  ( isDigit,
    isAlpha,
    isWhiteSpace,
    isAlphaNumeric,
  )
where

isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"

isAlpha :: Char -> Bool
isAlpha c = c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \r\t\n"

isAlphaNumeric :: Char -> Bool
isAlphaNumeric c = isDigit c || isAlpha c
