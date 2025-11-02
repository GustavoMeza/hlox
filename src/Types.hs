module Types (Token(..), ScanResult(..)) where

data Token = LeftParen
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

data ScanResult
  = TokenList [Token]
  | ScanError Int
  deriving (Eq, Show)
