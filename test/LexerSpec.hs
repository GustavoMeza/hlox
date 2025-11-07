module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "lex" $ do
    it "Parses empty string" $ do
      Lexer.lex "" `shouldBe` TokenList []
    it "Parses left parentheses" $ do
      Lexer.lex "(" `shouldBe` TokenList [LeftParen]
    it "Parses right parentheses" $ do
      Lexer.lex ")" `shouldBe` TokenList [RightParen]
    it "Parses left brace" $ do
      Lexer.lex "{" `shouldBe` TokenList [LeftBrace]
    it "Parses right brace" $ do
      Lexer.lex "}" `shouldBe` TokenList [RightBrace]
    it "Parses comma" $ do
      Lexer.lex "," `shouldBe` TokenList [Comma]
    it "Parses dot" $ do
      Lexer.lex "." `shouldBe` TokenList [Dot]
    it "Parses minus" $ do
      Lexer.lex "-" `shouldBe` TokenList [Minus]
    it "Parses plus" $ do
      Lexer.lex "+" `shouldBe` TokenList [Plus]
    it "Parses semicolon" $ do
      Lexer.lex ";" `shouldBe` TokenList [Semicolon]
    it "Parses star" $ do
      Lexer.lex "*" `shouldBe` TokenList [Star]
    it "Parses bang" $ do
      Lexer.lex "!" `shouldBe` TokenList [Bang]
    it "Parses bang equal" $ do
      Lexer.lex "!=" `shouldBe` TokenList [BangEqual]
    it "Parses equal" $ do
      Lexer.lex "=" `shouldBe` TokenList [Equal]
    it "Parses equal equal" $ do
      Lexer.lex "==" `shouldBe` TokenList [EqualEqual]
    it "Parses less" $ do
      Lexer.lex "<" `shouldBe` TokenList [Less]
    it "Parses less equal" $ do
      Lexer.lex "<=" `shouldBe` TokenList [LessEqual]
    it "Parses greater" $ do
      Lexer.lex ">" `shouldBe` TokenList [Greater]
    it "Parses greater equal" $ do
      Lexer.lex ">=" `shouldBe` TokenList [GreaterEqual]
    it "Parses slash" $ do
      Lexer.lex "/" `shouldBe` TokenList [Slash]
    it "Skips comments" $ do
      Lexer.lex "// A comment" `shouldBe` TokenList []
    it "Skips spaces" $ do
      Lexer.lex " " `shouldBe` TokenList []
    it "Skips returns" $ do
      Lexer.lex "\r" `shouldBe` TokenList []
    it "Skips tabs" $ do
      Lexer.lex "\t" `shouldBe` TokenList []
    it "Skips new lines" $ do
      Lexer.lex "\n" `shouldBe` TokenList []
    it "Parses strings" $ do
      Lexer.lex "\"Hello world\"" `shouldBe` TokenList [StringToken "Hello world"]
    it "Parses numbers" $ do
      Lexer.lex "1.23" `shouldBe` TokenList [Number 1.23]
    it "Parses and" $ do
      Lexer.lex "and" `shouldBe` TokenList [AndToken]
    it "Parses class" $ do
      Lexer.lex "class" `shouldBe` TokenList [ClassToken]
    it "Parses else" $ do
      Lexer.lex "else" `shouldBe` TokenList [ElseToken]
    it "Parses false" $ do
      Lexer.lex "false" `shouldBe` TokenList [FalseToken]
    it "Parses for" $ do
      Lexer.lex "for" `shouldBe` TokenList [ForToken]
    it "Parses fun" $ do
      Lexer.lex "fun" `shouldBe` TokenList [FunToken]
    it "Parses if" $ do
      Lexer.lex "if" `shouldBe` TokenList [IfToken]
    it "Parses nil" $ do
      Lexer.lex "nil" `shouldBe` TokenList [NilToken]
    it "Parses or" $ do
      Lexer.lex "or" `shouldBe` TokenList [OrToken]
    it "Parses print" $ do
      Lexer.lex "print" `shouldBe` TokenList [PrintToken]
    it "Parses return" $ do
      Lexer.lex "return" `shouldBe` TokenList [ReturnToken]
    it "Parses super" $ do
      Lexer.lex "super" `shouldBe` TokenList [SuperToken]
    it "Parses this" $ do
      Lexer.lex "this" `shouldBe` TokenList [ThisToken]
    it "Parses true" $ do
      Lexer.lex "true" `shouldBe` TokenList [TrueToken]
    it "Parses var" $ do
      Lexer.lex "var" `shouldBe` TokenList [VarToken]
    it "Parses while" $ do
      Lexer.lex "while" `shouldBe` TokenList [WhileToken]
    it "Parses identifiers" $ do
      Lexer.lex "a1_" `shouldBe` TokenList [Identifier "a1_"]
    it "Fails if strenous character" $ do
      Lexer.lex "@" `shouldBe` LexingError 0
    it "Counts line error" $ do
      Lexer.lex "\n\n\"Hello\nWorld" `shouldBe` LexingError 3
    it "Continues reading" $ do
      Lexer.lex
        "// this is a comment\n\
        \(( )){} // grouping stuff\n\
        \!*+-/=<> <= == // operators"
        `shouldBe` TokenList [LeftParen, LeftParen, RightParen, RightParen, LeftBrace, RightBrace, Bang, Star, Plus, Minus, Slash, Equal, Less, Greater, LessEqual, EqualEqual]
