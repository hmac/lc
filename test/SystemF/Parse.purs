module Test.SystemF.Parse where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Either (Either(..))

import SystemF.Parse (parseExpr)
import SystemF

main :: Effect Unit
main = do
  "x" ~> Var U "x"
  "y" ~> Var U "y"
  "x : T" ~> Var T "x"
  "x : A" ~> Var (TVar "A") "x"
  "(f : (T -> T)) (x : T)" ~> App U (Var (Arr T T) "f") (Var T "x")
  "f : T -> T x : T" ~> App U (Var (Arr T T) "f") (Var T "x")

  "f x" ~> App U (Var U "f") (Var U "x")
  "f (x : T)" ~> App U (Var U "f") (Var T "x")
  "f (x : T)" ~> App U (Var U "f") (Var T "x")

  "\\x. x" ~> Lam U "x" U (Var U "x")
  "λx. x" ~> Lam U "x" U (Var U "x")
  "\\x : T. x" ~> Lam U "x" T (Var U "x")
  "\\x : T. x" ~> Lam U "x" T (Var U "x")
  "\\f : T -> T. \\x : T. f x" ~>
    Lam U "f" (Arr T T) (Lam U "x" T (App U (Var U "f") (Var U "x")))

  "forall X. \\x : X. x" ~> Forall U "X" (Lam U "x" (TVar "X") (Var U "x"))
  "forall X. forall Y. \\x : X. \\y : Y. y" ~>
    Forall U "X" (Forall U "Y" (Lam U "x" (TVar "X") (Lam U "y" (TVar "Y") (Var U "y"))))
  "(forall X. \\x : X. x) T" ~> App U (Forall U "X" (Lam U "x" (TVar "X") (Var U "x"))) (Ty T)

  "0" ~> Forall U "X" (Lam U "f" (Arr (TVar "X") (TVar "X")) (Lam U "x" (TVar "X") (Var U "x")))
  "1" ~> Forall U "X" (Lam U "f" (Arr (TVar "X") (TVar "X")) (Lam U "x" (TVar "X") (App U (Var U "f") (Var U "x"))))

infixl 5 assertParse as ~>
assertParse :: String -> Expr -> Effect Unit
assertParse input expected =
  assertEqual { expected: Right expected, actual: parseExpr input }
