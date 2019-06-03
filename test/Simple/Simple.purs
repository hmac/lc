module Test.Simple where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual, assertTrue)
import Data.Either (Either(..), fromRight, isRight)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import Simple
import Expr
import Simple.Parse (parseExpr)

main :: Spec Unit
main = describe "Simple" do
  describe "inference" do
    Var U "x" ~ Var U "x"
    Fn U "x" T (Var U "x") ~ Fn (Arr T T) "x" T (Var T "x")
    App U (Var (Arr T T) "f") (Var T "x") ~ App T (Var (Arr T T) "f") (Var T "x")

  describe "reduction" do
    "(\\x : T. x) y" ~> "y"
    "(\\x : T. \\y : T. y) 1 2" ~> "2"
    -- Addition
    "(\\n : T -> T -> T. \\m : T -> T -> T. \\f : T -> T. \\x : T. n f (m f x)) 1 1" ~> "2"

-- TODO: test that every expression which typechecks also reduces to normal form

infixl 5 assertInfer as ~
assertInfer :: Expr -> Expr -> Spec Unit
assertInfer input expected =
  it ("infers a correct type for " <> show input) do
     infer mempty mempty input `shouldEqual` expected

infixl 5 assertParseAndReduce as ~>
assertParseAndReduce :: String -> String -> Spec Unit
assertParseAndReduce inputStr expectedStr =
  let expected = stripTypes <$> parseExpr expectedStr
      actual = nf mempty <<< infer mempty mempty <$> parseExpr inputStr
   in it ("correctly parses and reduces " <> inputStr) do
      isRight expected `shouldEqual` true
      actual `shouldEqual` expected
