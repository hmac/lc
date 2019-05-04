module Test.Simple where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)

import Simple
import Expr

main :: Effect Unit
main = do
  Var U "x" ~* Var U "x"

  Fn U "x" T (Var U "x") ~*
    Fn (Arr T T) "x" T (Var T "x")

  App U (Var (Arr T T) "f") (Var T "x") ~*
    App T (Var (Arr T T) "f") (Var T "x")

infixl 5 assertInfer as ~*
assertInfer :: Expr -> Expr -> Effect Unit
assertInfer input expected =
  assertEqual { expected: expected, actual: infer mempty input }
