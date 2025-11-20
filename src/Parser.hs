module Parser (ParsingError(..), ParsingState(..), ParsingResult(..), parseExpr) where

import Lexer
import Ast

data ParsingError = ParsingError deriving (Eq, Show)
data ParsingState = ParsingState {
  tokens :: [Token]
} deriving (Eq, Show)
type ParsingResult = Either ParsingError (ParsingState, Expr)

type Parser = ParsingState -> ParsingResult

parseExpr = parseEquality

parseBinaryExpr :: Parser -> Matcher BinaryOperator -> ParsingState -> ParsingResult
parseBinaryExpr parser matcher state = do
    (stateAfterFirstExpr, firstExpr) <- parser state
    parseRest firstExpr stateAfterFirstExpr
  where
    parseRest :: Expr -> ParsingState -> ParsingResult
    parseRest lhs state = 
      case match matcher state of
        Just (stateAfterOp, op) -> do
          (stateAfterRhs, rhs) <- parser stateAfterOp
          parseRest (BinaryExpr lhs op rhs) stateAfterRhs
        Nothing -> Right (state, lhs)

parseEquality = parseBinaryExpr parseComparison equalityOpMatcher
parseComparison = parseBinaryExpr parseTerm comparisonOpMatcher
parseTerm = parseBinaryExpr parseFactor termOpMatcher
parseFactor = parseBinaryExpr parseUnary factorOpMatcher

parseUnary :: Parser
parseUnary state =
  case match unaryOpMatcher state of
    Just (stateAfterOp, op) -> do
      (stateAfterUnary, rhs) <- parseUnary stateAfterOp
      return (stateAfterUnary, UnaryExpr op rhs)
    Nothing -> parsePrimary state

parsePrimary :: Parser
parsePrimary state =
  case match literalMatcher state of
    Just (stateAfterLiteral, literal) -> Right (stateAfterLiteral, LiteralExpr literal)
    Nothing -> case tokens state of
      LeftParen:tokensAfterLeftParen -> do
        (stateAfterExpr, expr) <- parseExpr (ParsingState tokensAfterLeftParen)
        case tokens stateAfterExpr of
          RightParen:tokensAfterRightExpr -> Right (ParsingState tokensAfterRightExpr, GroupingExpr expr)
          _ -> Left ParsingError 
      _ -> Left ParsingError

type Matcher a = (Token -> Maybe a)

equalityOpMatcher :: Matcher BinaryOperator
equalityOpMatcher token =
  case token of
    EqualEqual -> Just $ EqualEqualOperator
    BangEqual -> Just $ BangEqualOperator
    _ -> Nothing
  
comparisonOpMatcher :: Matcher BinaryOperator
comparisonOpMatcher token = 
  case token of
    Less -> Just $ LessOperator
    LessEqual -> Just $ LessEqualOperator
    Greater -> Just $ GreaterOperator
    GreaterEqual -> Just $ GreaterEqualOperator
    _ -> Nothing
  
termOpMatcher :: Matcher BinaryOperator
termOpMatcher token =
  case token of
    Plus -> Just $ PlusOperator
    Minus -> Just $ BinaryMinusOperator
    _ -> Nothing
  
factorOpMatcher :: Matcher BinaryOperator
factorOpMatcher token =
  case token of
    Star -> Just $ StarOperator
    Slash -> Just $ SlashOperator
    _ -> Nothing

unaryOpMatcher :: Matcher UnaryOperator
unaryOpMatcher token =
  case token of
    Bang -> Just $ BangOperator
    Minus -> Just $ UnaryMinusOperator
    _ -> Nothing

literalMatcher :: Matcher Literal
literalMatcher token =
  case token of
    Number number -> Just $ NumberLiteral number
    TrueToken -> Just $ BoolLiteral True
    FalseToken -> Just $ BoolLiteral False
    NilToken -> Just $ NilLiteral
    _ -> Nothing

match :: Matcher a -> ParsingState -> Maybe (ParsingState, a)
match matcher state =
  case tokens state of
    [] -> Nothing
    token:tokens -> case matcher token of
      Just a -> Just (ParsingState tokens, a)
      Nothing -> Nothing
