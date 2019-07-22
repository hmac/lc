module Test.Dependent where


import Prelude
import Data.List as List
import Effect (Effect)
import Data.Either (Either(..), fromRight, isRight)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import Dependent.Parse (parseExpr)
import Dependent.Dependent

main :: Spec Unit
main = describe "lambda pi" do
  describe "parsing" do
    "(\\n. \\m. natElim (\\_. Nat) n (\\k. \\rec. S rec) m) : forall (n : Nat).  forall (m : Nat). Nat" ~
      Ann (Lam "n" (Lam "m" (NatElim (Lam "_" Nat) (Var "n") (Lam "k" (Lam "rec" (Succ (Var "rec")))) (Var "m")))) (Pi "n" Nat (Pi "m" Nat Nat))
    "(\\a. \\k1. \\v1. \\k2. \\v2. vecElim a (\\m. \\_. Vec a (plus k1 m)) v1 (\\m. \\v.  \\vs. \\rec. VCons a (S m) v rec) k2 v2) : forall (a : Type). forall (k1 : Nat). forall (v1 : Vec a k1). forall (k2 : Nat). forall (v2: Vec a k2). Vec a (plus k1 k2)" ~
      Ann (Lam "a" (Lam "k1" (Lam "v1" (Lam "k2" (Lam "v2" (VecElim (Var "a") (Lam "m" (Lam "_" (Vec (Var "a") (App (App (Var "plus") (Var "k1")) (Var "m"))))) (Var "v1") (Lam "m" (Lam "v" (Lam "vs" (Lam "rec" (VCons (Var "a") (Succ (Var "m")) (Var "v") (Var "rec")))))) (Var "k2") (Var "v2"))))))) (Pi "a" Type (Pi "k1" Nat (Pi "v1" (Vec (Var "a") (Var "k1")) (Pi "k2" Nat (Pi "v2" (Vec (Var "a") (Var "k2")) (Vec (Var "a") (App (App (Var "plus") (Var "k1")) (Var "k2"))))))))
    "(\\t. \\a. VCons t Z a (VNil t)) : forall (t : Type). forall (a : t). Vec t (S Z)" ~
      Ann (Lam "t" (Lam "a" (VCons (Var "t") Zero (Var "a") (VNil (Var "t"))))) (Pi "t" Type (Pi "a" (Var "t") (Vec (Var "t") (Succ Zero))))
    "(\\t. \\x. x) : forall (t : Type). forall (x : t). t" ~
      Ann (Lam "t" (Lam "x" (Var "x"))) (Pi "t" Type (Pi "x" (Var "t") (Var "t")))

  describe "evaluation" do
    "((\\t. \\k. \\v. \\d. vecElim t (\\_. \\_. t) d (\\l. \\x. \\xs. \\rec. x) k v) : forall (t : Type). forall (k : Nat). forall (v : Vec t k). forall (default : t). t) Nat (S (S Z)) (VCons Nat (S Z) (S S Z) (VCons Nat Z (S Z) (VNil Nat))) (S S S S Z)" ~>
      "S S Z"


infixl 5 assertParse as ~
assertParse :: String -> Expr -> Spec Unit
assertParse inputStr expected =
  let actual = parseExpr inputStr
   in
     it ("parses  " <> inputStr) do
        actual `shouldEqual` Right expected

infixl 5 assertParseAndReduce as ~>
assertParseAndReduce :: String -> String -> Spec Unit
assertParseAndReduce inputStr expectedStr =
  let actual = nf <$> parseExpr inputStr
      expected = parseExpr expectedStr
   in
     it ("parses and correctly reduces " <> inputStr) do
        isRight expected `shouldEqual` true
        actual `shouldEqual` expected
