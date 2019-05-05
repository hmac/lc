module Test.Simple where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual, assertTrue)
import Data.Either (Either(..), fromRight, isRight)

import Simple
import Expr
import Simple.Parse (parseExpr)

main :: Effect Unit
main = do
  -- Inference
  Var U "x" ~ Var U "x"
  Fn U "x" T (Var U "x") ~ Fn (Arr T T) "x" T (Var T "x")
  App U (Var (Arr T T) "f") (Var T "x") ~ App T (Var (Arr T T) "f") (Var T "x")

  -- Reduction
  "(\\x : T. x) y" ~> "y"
  "(\\x : T. \\y : T. y) 1 2" ~> "2"
  -- Addition
  "(\\n : T -> T -> T. \\m : T -> T -> T. \\f : T -> T. \\x : T. n f (m f x)) 1 1" ~> "2"

-- TODO: test that every expression which typechecks also reduces to normal form

infixl 5 assertInfer as ~
assertInfer :: Expr -> Expr -> Effect Unit
assertInfer input expected =
  assertEqual { expected: expected, actual: infer mempty mempty input }

infixl 5 assertParseAndReduce as ~>
assertParseAndReduce :: String -> String -> Effect Unit
assertParseAndReduce inputStr expectedStr =
  let expected = stripTypes <$> parseExpr expectedStr
      actual = nf mempty <<< infer mempty mempty <$> parseExpr inputStr
   in do
      assertTrue (isRight expected)
      assertEqual { expected: expected, actual: actual }
