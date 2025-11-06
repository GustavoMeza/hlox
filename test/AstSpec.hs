module AstSpec (spec) where

import Ast
import Test.Hspec

spec :: Spec
spec = do
  describe "Ast" $ do
    it "Compiles an Ast" $ do
      let ast = BinaryExpr (UnaryExpr UnaryMinusOperator (LiteralExpr (NumberLiteral 123))) StarOperator (GroupingExpr (LiteralExpr (NumberLiteral 45.67))) :: Expr
      show ast `shouldBe` "(* (- 123.0) (group 45.67))"
