module Test.HM where

import Prelude
import Data.List as List
import Effect (Effect)
import Data.Either (Either(..), fromRight, isRight)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import HM
import HM.Parse (parseExpr)

main :: Spec Unit
main = describe "Hindley-Milner" do
  describe "inference" do
    Let "x" Unit (Var "x") ~ T
    Let "f" (Lam "x" (Lam "y" (Var "y"))) (Lam "x" (App (App (Var "f") (Var "x")) (Var "x"))) ~ (Arr (TVar "d") (TVar "d"))
    Lam "x" (Var "x") ~ Arr (TVar "a") (TVar "a")
    Let "f" (Lam "t" Unit) (Let "x" (Lam "t" (Var "t")) (App (Var "f") (Var "x"))) ~ T

    "let x = unit in x" ~> T
    "let f = (\\x. \\y. y) in \\x. f x x" ~> Arr (TVar "d") (TVar "d")
    "\\x. x" ~> Arr (TVar "a") (TVar "a")
    "let f = (\\t. unit) in let x = (\\t. t) in f x" ~> T

infixl 5 assertInfer as ~
assertInfer :: Expr -> Type -> Spec Unit
assertInfer input expected =
  it "infers the correct type" do
     runInfer' mempty input `shouldEqual` Right expected

infixl 5 assertParseAndInfer as ~>
assertParseAndInfer :: String -> Type -> Spec Unit
assertParseAndInfer inputStr expected =
  let actual = runInfer' mempty <$> parseExpr inputStr
   in
     it "parses and reduces to the correct result" do
        actual `shouldEqual` Right (Right expected)
