module Test.SystemF where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual, assertTrue)
import Data.Either (Either(..), fromRight, isRight)

import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import SystemF
import SystemF.Parse (parseExpr)

import Pretty

main :: Spec Unit
main = describe "System F" do
  -- Inference
  describe "inference" do
    Var U "x" ~ Var U "x"
    Lam U "x" T (Var U "x") ~ Lam (Arr T T) "x" T (Var T "x")
    App U (Var (Arr T T) "f") (Var T "x") ~ App T (Var (Arr T T) "f") (Var T "x")

  describe "reduction" do
    "(\\x : T. x) y" ~> "y"
    "(\\x : T. \\y : T. y) 1 2" ~> "2"

    "0 T" ~> "0 T"
  -- Addition
    {-- "(\\n : T -> T -> T. \\m : T -> T -> T. \\f : T -> T. \\x : T. n f (m f x)) (1 T) (1 T)" --}
    {--   ~> "2 T" --}

-- TODO: test that every expression which typechecks also reduces to normal form

infixl 5 assertInfer as ~
assertInfer :: Expr -> Expr -> Spec Unit
assertInfer input expected =
  it ("infers the correct type of " <> pretty input) do
    infer mempty mempty input `shouldEqual` expected

infixl 5 assertParseAndReduce as ~>
assertParseAndReduce :: String -> String -> Spec Unit
assertParseAndReduce inputStr expectedStr =
  let expected = nf mempty <<< infer mempty mempty <$> parseExpr expectedStr
      actual = nf mempty <<< infer mempty mempty <$> parseExpr inputStr
   in it ("correctly parses and reduces " <> inputStr) do
        isRight expected `shouldEqual` true
        actual `shouldEqual` expected
