module Test.HM where

import Prelude
import Data.List as List
import Effect (Effect)
import Data.Either (Either(..), fromRight, isRight)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import HM
import HM.Parse (parseExpr)
import HM.Eval (nf)

main :: Spec Unit
main = describe "Hindley-Milner" do
  describe "inference" do
    "let x = unit in x" ~ T
    "let f = (\\x. \\y. y) in \\x. f x x" ~ Arr (TVar "d") (TVar "d")
    "\\x. x" ~ Arr (TVar "a") (TVar "a")
    "let f = (\\t. unit) in let x = (\\t. t) in f x" ~ T
    "let x = True in (\\x. x) x" ~ TBool
    "let x = 1 in x" ~ TInt
    "let add = #add in (add 1 2)" ~ TInt
    "let mul = #mul in (mul 1 2)" ~ TInt

  describe "reduction" do
    "True" ~> "True"
    "let f = (\\t. unit) in let x = (\\t. t) in f x" ~> "unit"
    "let const = (\\x. \\y. x) in const True False" ~> "True"
    "let add = #add in (add 1 2)" ~> "3"
    "let x = 1 in let add = #add in (add (add x x) x)" ~> "3"
    "let mul = #mul in (mul 1 2)" ~> "2"


infixl 5 assertParseAndInfer as ~
assertParseAndInfer :: String -> Type -> Spec Unit
assertParseAndInfer inputStr expected =
  let actual = runInfer' mempty <$> parseExpr inputStr
   in
     it ("parses and correctly infers " <> inputStr) do
        actual `shouldEqual` Right (Right expected)

infixl 5 assertParseAndReduce as ~>
assertParseAndReduce :: String -> String -> Spec Unit
assertParseAndReduce inputStr expectedStr =
  let actual = nf mempty <$> parseExpr inputStr
      expected = parseExpr expectedStr
   in
     it ("parses and correctly reduces " <> inputStr) do
        isRight expected `shouldEqual` true
        actual `shouldEqual` expected
