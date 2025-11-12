module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "lex" $ do
    it "Parses empty string" $ do
      Lexer.lex "" `shouldBe` Right []
    it "Parses left parentheses" $ do
      Lexer.lex "(" `shouldBe` Right [LeftParen]
    it "Parses right parentheses" $ do
      Lexer.lex ")" `shouldBe` Right [RightParen]
    it "Parses left brace" $ do
      Lexer.lex "{" `shouldBe` Right [LeftBrace]
    it "Parses right brace" $ do
      Lexer.lex "}" `shouldBe` Right [RightBrace]
    it "Parses comma" $ do
      Lexer.lex "," `shouldBe` Right [Comma]
    it "Parses dot" $ do
      Lexer.lex "." `shouldBe` Right [Dot]
    it "Parses minus" $ do
      Lexer.lex "-" `shouldBe` Right [Minus]
    it "Parses plus" $ do
      Lexer.lex "+" `shouldBe` Right [Plus]
    it "Parses semicolon" $ do
      Lexer.lex ";" `shouldBe` Right [Semicolon]
    it "Parses star" $ do
      Lexer.lex "*" `shouldBe` Right [Star]
    it "Parses bang" $ do
      Lexer.lex "!" `shouldBe` Right [Bang]
    it "Parses bang equal" $ do
      Lexer.lex "!=" `shouldBe` Right [BangEqual]
    it "Parses equal" $ do
      Lexer.lex "=" `shouldBe` Right [Equal]
    it "Parses equal equal" $ do
      Lexer.lex "==" `shouldBe` Right [EqualEqual]
    it "Parses less" $ do
      Lexer.lex "<" `shouldBe` Right [Less]
    it "Parses less equal" $ do
      Lexer.lex "<=" `shouldBe` Right [LessEqual]
    it "Parses greater" $ do
      Lexer.lex ">" `shouldBe` Right [Greater]
    it "Parses greater equal" $ do
      Lexer.lex ">=" `shouldBe` Right [GreaterEqual]
    it "Parses slash" $ do
      Lexer.lex "/" `shouldBe` Right [Slash]
    it "Skips comments" $ do
      Lexer.lex "// A comment" `shouldBe` Right []
    it "Skips spaces" $ do
      Lexer.lex " " `shouldBe` Right []
    it "Skips returns" $ do
      Lexer.lex "\r" `shouldBe` Right []
    it "Skips tabs" $ do
      Lexer.lex "\t" `shouldBe` Right []
    it "Skips new lines" $ do
      Lexer.lex "\n" `shouldBe` Right []
    it "Parses strings" $ do
      Lexer.lex "\"Hello world\"" `shouldBe` Right [StringToken "Hello world"]
    it "Parses numbers" $ do
      Lexer.lex "1.23" `shouldBe` Right [Number 1.23]
    it "Parses and" $ do
      Lexer.lex "and" `shouldBe` Right [AndToken]
    it "Parses class" $ do
      Lexer.lex "class" `shouldBe` Right [ClassToken]
    it "Parses else" $ do
      Lexer.lex "else" `shouldBe` Right [ElseToken]
    it "Parses false" $ do
      Lexer.lex "false" `shouldBe` Right [FalseToken]
    it "Parses for" $ do
      Lexer.lex "for" `shouldBe` Right [ForToken]
    it "Parses fun" $ do
      Lexer.lex "fun" `shouldBe` Right [FunToken]
    it "Parses if" $ do
      Lexer.lex "if" `shouldBe` Right [IfToken]
    it "Parses nil" $ do
      Lexer.lex "nil" `shouldBe` Right [NilToken]
    it "Parses or" $ do
      Lexer.lex "or" `shouldBe` Right [OrToken]
    it "Parses print" $ do
      Lexer.lex "print" `shouldBe` Right [PrintToken]
    it "Parses return" $ do
      Lexer.lex "return" `shouldBe` Right [ReturnToken]
    it "Parses super" $ do
      Lexer.lex "super" `shouldBe` Right [SuperToken]
    it "Parses this" $ do
      Lexer.lex "this" `shouldBe` Right [ThisToken]
    it "Parses true" $ do
      Lexer.lex "true" `shouldBe` Right [TrueToken]
    it "Parses var" $ do
      Lexer.lex "var" `shouldBe` Right [VarToken]
    it "Parses while" $ do
      Lexer.lex "while" `shouldBe` Right [WhileToken]
    it "Parses identifiers" $ do
      Lexer.lex "a1_" `shouldBe` Right [Identifier "a1_"]
    it "Fails if strenous character" $ do
      Lexer.lex "@" `shouldBe` Left [LexingError 0]
    it "Counts line error" $ do
      Lexer.lex "\n\n\"Hello\nWorld" `shouldBe` Left [LexingError 3]
    it "Continues reading" $ do
      Lexer.lex
        "// this is a comment\n\
        \(( )){} // grouping stuff\n\
        \!*+-/=<> <= == // operators"
        `shouldBe` Right [LeftParen, LeftParen, RightParen, RightParen, LeftBrace, RightBrace, Bang, Star, Plus, Minus, Slash, Equal, Less, Greater, LessEqual, EqualEqual]
