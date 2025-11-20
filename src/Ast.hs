module Ast
  ( Expr (..),
    UnaryOperator (..),
    BinaryOperator (..),
    Literal (..),
  )
where

import Data.List (intercalate)

data Expr
  = LiteralExpr Literal
  | UnaryExpr UnaryOperator Expr
  | BinaryExpr Expr BinaryOperator Expr
  | GroupingExpr Expr
  deriving (Eq)

data UnaryOperator
  = UnaryMinusOperator
  | BangOperator
  deriving (Eq)

data BinaryOperator
  = EqualEqualOperator
  | BangEqualOperator
  | LessOperator
  | LessEqualOperator
  | GreaterOperator
  | GreaterEqualOperator
  | PlusOperator
  | BinaryMinusOperator
  | StarOperator
  | SlashOperator
  deriving (Eq)

data Literal
  = NumberLiteral Double
  | StringLiteral String
  | BoolLiteral Bool
  | NilLiteral
  deriving (Eq)

instance Show Expr where
  show (LiteralExpr literal) = show literal
  show (UnaryExpr operator expr) = parenthesize [show operator, show expr]
  show (BinaryExpr lhsExpr operator rhsExpr) = parenthesize [show operator, show lhsExpr, show rhsExpr]
  show (GroupingExpr expr) = parenthesize ["group", show expr]

instance Show UnaryOperator where
  show UnaryMinusOperator = "-"
  show BangOperator = "!"

instance Show BinaryOperator where
  show EqualEqualOperator = "=="
  show BangEqualOperator = "!="
  show LessOperator = "<"
  show LessEqualOperator = "<="
  show GreaterOperator = ">"
  show GreaterEqualOperator = ">="
  show PlusOperator = "+"
  show BinaryMinusOperator = "-"
  show StarOperator = "*"
  show SlashOperator = "/"

instance Show Literal where
  show (NumberLiteral value) = show value
  show (StringLiteral value) = value
  show (BoolLiteral value) = show value
  show NilLiteral = "nil"

parenthesize :: [String] -> String
parenthesize words = "(" ++ (intercalate " " words) ++ ")"
