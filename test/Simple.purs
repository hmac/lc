module Test.Simple where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)

import Simple
import Expr

main :: Effect Unit
main = do
  Var U "x" ~* Var U "x"

  Fn U "x" (T "A") (Var U "x") ~*
    Fn (Arr (T "A") (T "A")) "x" (T "A") (Var (T "A") "x")

  App U (Var (Arr (T "A") (T "B")) "f") (Var (T "A") "x") ~*
    App (T "B") (Var (Arr (T "A") (T "B")) "f") (Var (T "A") "x")

infixl 5 assertInfer as ~*
assertInfer :: Expr -> Expr -> Effect Unit
assertInfer input expected =
  assertEqual { expected: expected, actual: infer mempty input }
