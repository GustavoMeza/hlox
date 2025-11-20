module ParserSpec (spec) where

import Ast
import Lexer
import Parser
import Test.Hspec
import Data.Either (isLeft)

spec :: Spec
spec = do
  describe "single token parsing" $ do
    it "Parses true" $ do
      parseExpr (ParsingState [TrueToken]) `shouldBe` Right (ParsingState [], LiteralExpr $ BoolLiteral True)
    it "Parses false" $ do
      parseExpr (ParsingState [FalseToken]) `shouldBe` Right (ParsingState [], LiteralExpr $ BoolLiteral False)
    it "Parses nil" $ do
      parseExpr (ParsingState [NilToken]) `shouldBe` Right (ParsingState [], LiteralExpr $ NilLiteral)
    it "Parses number" $ do
      parseExpr (ParsingState [Number 4.20]) `shouldBe` Right (ParsingState [], LiteralExpr $ NumberLiteral 4.20)
    it "Parses grouping" $ do
      parseExpr (ParsingState [LeftParen, Number 1.0, Plus, Number 2.0, RightParen]) `shouldBe` Right (ParsingState [], GroupingExpr $ BinaryExpr (LiteralExpr $ NumberLiteral 1.0) PlusOperator (LiteralExpr $ NumberLiteral 2.0))
    it "Parses unary minus" $ do
      parseExpr (ParsingState [Minus, Number 3.4]) `shouldBe` Right (ParsingState [], UnaryExpr UnaryMinusOperator $ LiteralExpr $ NumberLiteral 3.4)
    it "Parses bang" $ do
      parseExpr (ParsingState [Bang, TrueToken]) `shouldBe` Right (ParsingState [], UnaryExpr BangOperator $ LiteralExpr $ BoolLiteral True)
    it "Parses product" $ do
      parseExpr (ParsingState [Number 1.2, Star, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) StarOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses division" $ do
      parseExpr (ParsingState [Number 1.2, Slash, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) SlashOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses addition" $ do
      parseExpr (ParsingState [Number 1.2, Plus, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) PlusOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses substraction" $ do
      parseExpr (ParsingState [Number 1.2, Minus, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) BinaryMinusOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses lt" $ do
      parseExpr (ParsingState [Number 1.2, Less, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) LessOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses lte" $ do
      parseExpr (ParsingState [Number 1.2, LessEqual, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) LessEqualOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses gt" $ do
      parseExpr (ParsingState [Number 1.2, Greater, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) GreaterOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses gte" $ do
      parseExpr (ParsingState [Number 1.2, GreaterEqual, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) GreaterEqualOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses equal" $ do
      parseExpr (ParsingState [Number 1.2, EqualEqual, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) EqualEqualOperator (LiteralExpr $ NumberLiteral 3.4)) 
    it "Parses not equal" $ do
      parseExpr (ParsingState [Number 1.2, BangEqual, Number 3.4]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.2) BangEqualOperator (LiteralExpr $ NumberLiteral 3.4)) 
  describe "associativity" $ do
    it "Unaries are right associative" $ do
      parseExpr (ParsingState [Bang, Minus, TrueToken]) `shouldBe` Right (ParsingState [], UnaryExpr BangOperator $ UnaryExpr UnaryMinusOperator $ LiteralExpr $ BoolLiteral True)
    it "Factors are left associative" $ do
      parseExpr (ParsingState [Number 1.0, Star, Number 2.0, Slash, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (BinaryExpr (LiteralExpr $ NumberLiteral 1.0) StarOperator (LiteralExpr $ NumberLiteral 2.0)) SlashOperator (LiteralExpr $ NumberLiteral 3.0)) 
    it "Terms are left associative" $ do
      parseExpr (ParsingState [Number 1.0, Plus, Number 2.0, Minus, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (BinaryExpr (LiteralExpr $ NumberLiteral 1.0) PlusOperator (LiteralExpr $ NumberLiteral 2.0)) BinaryMinusOperator (LiteralExpr $ NumberLiteral 3.0)) 
    it "Comparisons are left associative" $ do
      parseExpr (ParsingState [Number 1.0, Less, Number 2.0, Greater, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (BinaryExpr (LiteralExpr $ NumberLiteral 1.0) LessOperator (LiteralExpr $ NumberLiteral 2.0)) GreaterOperator (LiteralExpr $ NumberLiteral 3.0)) 
    it "Equalities are left associative" $ do
      parseExpr (ParsingState [Number 1.0, EqualEqual, Number 2.0, BangEqual, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (BinaryExpr (LiteralExpr $ NumberLiteral 1.0) EqualEqualOperator (LiteralExpr $ NumberLiteral 2.0)) BangEqualOperator (LiteralExpr $ NumberLiteral 3.0)) 
  describe "precedence" $ do
    it "Unaries have precedence over factors" $ do
      parseExpr (ParsingState [Minus, Number 1.0, Star, Number 2.0]) `shouldBe` Right (ParsingState [], BinaryExpr (UnaryExpr UnaryMinusOperator (LiteralExpr $ NumberLiteral 1.0)) StarOperator (LiteralExpr $ NumberLiteral 2.0)) 
    it "Factors have precedence over Terms" $ do
      parseExpr (ParsingState [Number 1.0, Plus, Number 2.0, Star, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.0) PlusOperator (BinaryExpr (LiteralExpr $ NumberLiteral 2.0) StarOperator (LiteralExpr $ NumberLiteral 3.0))) 
    it "Terms have precedence over Comparisons" $ do
      parseExpr (ParsingState [Number 1.0, Less, Number 2.0, Plus, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.0) LessOperator (BinaryExpr (LiteralExpr $ NumberLiteral 2.0) PlusOperator (LiteralExpr $ NumberLiteral 3.0))) 
    it "Comparisons have precedence over Equalities" $ do
      parseExpr (ParsingState [Number 1.0, EqualEqual, Number 2.0, Less, Number 3.0]) `shouldBe` Right (ParsingState [], BinaryExpr (LiteralExpr $ NumberLiteral 1.0) EqualEqualOperator (BinaryExpr (LiteralExpr $ NumberLiteral 2.0) LessOperator (LiteralExpr $ NumberLiteral 3.0))) 
  describe "fails gracefully" $ do
    it "Empty expr returns error" $ do
      parseExpr (ParsingState []) `shouldSatisfy` isLeft
    it "Missing parentheses returns error" $ do
      parseExpr (ParsingState [LeftParen, Number 1.0]) `shouldSatisfy` isLeft
