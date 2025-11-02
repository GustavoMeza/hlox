module ScannerSpec (spec) where

import Test.Hspec
import Scanner (scan)
import Types (Token(..), ScanResult(..))

spec :: Spec
spec = do
  describe "scan" $ do
    it "Parses empty string" $ do
      scan "" `shouldBe` TokenList []
    it "Parses left parentheses" $ do
      scan "(" `shouldBe` TokenList [LeftParen]
    it "Parses right parentheses" $ do
      scan ")" `shouldBe` TokenList [RightParen]
    it "Parses left brace" $ do
      scan "{" `shouldBe` TokenList [LeftBrace]
    it "Parses right brace" $ do
      scan "}" `shouldBe` TokenList [RightBrace]
    it "Parses comma" $ do
      scan "," `shouldBe` TokenList [Comma]
    it "Parses dot" $ do
      scan "." `shouldBe` TokenList [Dot]
    it "Parses minus" $ do
      scan "-" `shouldBe` TokenList [Minus]
    it "Parses plus" $ do
      scan "+" `shouldBe` TokenList [Plus]
    it "Parses semicolon" $ do
      scan ";" `shouldBe` TokenList [Semicolon]
    it "Parses star" $ do
      scan "*" `shouldBe` TokenList [Star]
    it "Parses bang" $ do
      scan "!" `shouldBe` TokenList [Bang]
    it "Parses bang equal" $ do
      scan "!=" `shouldBe` TokenList [BangEqual]
    it "Parses equal" $ do
      scan "=" `shouldBe` TokenList [Equal]
    it "Parses equal equal" $ do
      scan "==" `shouldBe` TokenList [EqualEqual]
    it "Parses less" $ do
      scan "<" `shouldBe` TokenList [Less]
    it "Parses less equal" $ do
      scan "<=" `shouldBe` TokenList [LessEqual]
    it "Parses greater" $ do
      scan ">" `shouldBe` TokenList [Greater]
    it "Parses greater equal" $ do
      scan ">=" `shouldBe` TokenList [GreaterEqual]
    it "Parses slash" $ do
      scan "/" `shouldBe` TokenList [Slash]
    it "Skips comments" $ do
      scan "// A comment" `shouldBe` TokenList [] 
    it "Skips spaces" $ do
      scan " " `shouldBe` TokenList []
    it "Skips returns" $ do
      scan "\r" `shouldBe` TokenList []
    it "Skips tabs" $ do
      scan "\t" `shouldBe` TokenList []
    it "Skips new lines" $ do
      scan "\n" `shouldBe` TokenList []
    it "Parses strings" $ do
      scan "\"Hello world\"" `shouldBe` TokenList [StringToken "Hello world"]
    it "Parses numbers" $ do
      scan "1.23" `shouldBe` TokenList [Number  1.23]
    it "Parses and" $ do
      scan "and" `shouldBe` TokenList [AndToken]
    it "Parses class" $ do
      scan "class" `shouldBe` TokenList [ClassToken]
    it "Parses else" $ do
      scan "else" `shouldBe` TokenList [ElseToken]
    it "Parses false" $ do
      scan "false" `shouldBe` TokenList [FalseToken]
    it "Parses for" $ do
      scan "for" `shouldBe` TokenList [ForToken]
    it "Parses fun" $ do
      scan "fun" `shouldBe` TokenList [FunToken]
    it "Parses if" $ do
      scan "if" `shouldBe` TokenList [IfToken]
    it "Parses nil" $ do
      scan "nil" `shouldBe` TokenList [NilToken]
    it "Parses or" $ do
      scan "or" `shouldBe` TokenList [OrToken]
    it "Parses print" $ do
      scan "print" `shouldBe` TokenList [PrintToken]
    it "Parses return" $ do
      scan "return" `shouldBe` TokenList [ReturnToken]
    it "Parses super" $ do
      scan "super" `shouldBe` TokenList [SuperToken]
    it "Parses this" $ do
      scan "this" `shouldBe` TokenList [ThisToken]
    it "Parses true" $ do
      scan "true" `shouldBe` TokenList [TrueToken]
    it "Parses var" $ do
      scan "var" `shouldBe` TokenList [VarToken]
    it "Parses while" $ do
      scan "while" `shouldBe` TokenList [WhileToken]
    it "Parses identifiers" $ do
      scan "a1_" `shouldBe` TokenList [Identifier "a1_"]
    it "Fails if strenous character" $ do
      scan "@" `shouldBe` ScanError 0
    it "Counts line error" $ do
      scan "\n\n\"Hello\nWorld" `shouldBe` ScanError 3
    it "Continues reading" $ do
      scan "// this is a comment\n\
           \(( )){} // grouping stuff\n\
           \!*+-/=<> <= == // operators" `shouldBe` TokenList [LeftParen, LeftParen, RightParen, RightParen, LeftBrace, RightBrace, Bang, Star, Plus, Minus, Slash, Equal, Less, Greater, LessEqual, EqualEqual]
