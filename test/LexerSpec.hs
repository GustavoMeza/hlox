module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "lex" $ do
    it "Parses empty string" $ do
      Lexer.lex "" `shouldBe` Right [End]
    it "Parses left parentheses" $ do
      Lexer.lex "(" `shouldBe` Right [LeftParen, End]
    it "Parses right parentheses" $ do
      Lexer.lex ")" `shouldBe` Right [RightParen, End]
    it "Parses left brace" $ do
      Lexer.lex "{" `shouldBe` Right [LeftBrace, End]
    it "Parses right brace" $ do
      Lexer.lex "}" `shouldBe` Right [RightBrace, End]
    it "Parses comma" $ do
      Lexer.lex "," `shouldBe` Right [Comma, End]
    it "Parses dot" $ do
      Lexer.lex "." `shouldBe` Right [Dot, End]
    it "Parses minus" $ do
      Lexer.lex "-" `shouldBe` Right [Minus, End]
    it "Parses plus" $ do
      Lexer.lex "+" `shouldBe` Right [Plus, End]
    it "Parses semicolon" $ do
      Lexer.lex ";" `shouldBe` Right [Semicolon, End]
    it "Parses star" $ do
      Lexer.lex "*" `shouldBe` Right [Star, End]
    it "Parses bang" $ do
      Lexer.lex "!" `shouldBe` Right [Bang, End]
    it "Parses bang equal" $ do
      Lexer.lex "!=" `shouldBe` Right [BangEqual, End]
    it "Parses equal" $ do
      Lexer.lex "=" `shouldBe` Right [Equal, End]
    it "Parses equal equal" $ do
      Lexer.lex "==" `shouldBe` Right [EqualEqual, End]
    it "Parses less" $ do
      Lexer.lex "<" `shouldBe` Right [Less, End]
    it "Parses less equal" $ do
      Lexer.lex "<=" `shouldBe` Right [LessEqual, End]
    it "Parses greater" $ do
      Lexer.lex ">" `shouldBe` Right [Greater, End]
    it "Parses greater equal" $ do
      Lexer.lex ">=" `shouldBe` Right [GreaterEqual, End]
    it "Parses slash" $ do
      Lexer.lex "/" `shouldBe` Right [Slash, End]
    it "Skips comments" $ do
      Lexer.lex "// A comment" `shouldBe` Right [End]
    it "Skips spaces" $ do
      Lexer.lex " " `shouldBe` Right [End]
    it "Skips returns" $ do
      Lexer.lex "\r" `shouldBe` Right [End]
    it "Skips tabs" $ do
      Lexer.lex "\t" `shouldBe` Right [End]
    it "Skips new lines" $ do
      Lexer.lex "\n" `shouldBe` Right [End]
    it "Parses strings" $ do
      Lexer.lex "\"Hello world\"" `shouldBe` Right [StringToken "Hello world", End]
    it "Parses numbers" $ do
      Lexer.lex "1.23" `shouldBe` Right [Number 1.23, End]
    it "Parses and" $ do
      Lexer.lex "and" `shouldBe` Right [AndToken, End]
    it "Parses class" $ do
      Lexer.lex "class" `shouldBe` Right [ClassToken, End]
    it "Parses else" $ do
      Lexer.lex "else" `shouldBe` Right [ElseToken, End]
    it "Parses false" $ do
      Lexer.lex "false" `shouldBe` Right [FalseToken, End]
    it "Parses for" $ do
      Lexer.lex "for" `shouldBe` Right [ForToken, End]
    it "Parses fun" $ do
      Lexer.lex "fun" `shouldBe` Right [FunToken, End]
    it "Parses if" $ do
      Lexer.lex "if" `shouldBe` Right [IfToken, End]
    it "Parses nil" $ do
      Lexer.lex "nil" `shouldBe` Right [NilToken, End]
    it "Parses or" $ do
      Lexer.lex "or" `shouldBe` Right [OrToken, End]
    it "Parses print" $ do
      Lexer.lex "print" `shouldBe` Right [PrintToken, End]
    it "Parses return" $ do
      Lexer.lex "return" `shouldBe` Right [ReturnToken, End]
    it "Parses super" $ do
      Lexer.lex "super" `shouldBe` Right [SuperToken, End]
    it "Parses this" $ do
      Lexer.lex "this" `shouldBe` Right [ThisToken, End]
    it "Parses true" $ do
      Lexer.lex "true" `shouldBe` Right [TrueToken, End]
    it "Parses var" $ do
      Lexer.lex "var" `shouldBe` Right [VarToken, End]
    it "Parses while" $ do
      Lexer.lex "while" `shouldBe` Right [WhileToken, End]
    it "Parses identifiers" $ do
      Lexer.lex "a1_" `shouldBe` Right [Identifier "a1_", End]
    it "Fails if strenous character" $ do
      Lexer.lex "@" `shouldBe` Left (LexingError 0)
    it "Counts line error" $ do
      Lexer.lex "\n\n\"Hello\nWorld" `shouldBe` Left (LexingError 3)
    it "Continues reading" $ do
      Lexer.lex
        "// this is a comment\n\
        \(( )){} // grouping stuff\n\
        \!*+-/=<> <= == // operators"
        `shouldBe` Right [LeftParen, LeftParen, RightParen, RightParen, LeftBrace, RightBrace, Bang, Star, Plus, Minus, Slash, Equal, Less, Greater, LessEqual, EqualEqual, End]
