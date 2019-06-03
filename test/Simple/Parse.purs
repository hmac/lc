module Test.Simple.Parse where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Either (Either(..))
import Test.QuickCheck (class Arbitrary, arbitrary, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Data.NonEmpty ((:|))
import Control.Lazy (defer)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

import Data.String.Gen (genAlphaString)

import Simple.Parse (parseExpr)
import Expr
import Simple (Expr, Type(..))

main :: Spec Unit
main = describe "Simple" do
  describe "Parsing" do
    "x" ~> Var U "x"
    "y" ~> Var U "y"
    "x : T" ~> Var T "x"
    "x : A" ~> Var (C "A") "x"
    "(f : (T -> T)) (x : T)" ~> App U (Var (Arr T T) "f") (Var T "x")
    "f : T -> T x : T" ~> App U (Var (Arr T T) "f") (Var T "x")

    "f x" ~> App U (Var U "f") (Var U "x")
    "f (x : T)" ~> App U (Var U "f") (Var T "x")
    "f (x : T)" ~> App U (Var U "f") (Var T "x")

    "\\x. x" ~> Fn U "x" U (Var U "x")
    "λx. x" ~> Fn U "x" U (Var U "x")
    "\\x : T. x" ~> Fn U "x" T (Var U "x")
    "\\x : T. x" ~> Fn U "x" T (Var U "x")
    "\\f : T -> T. \\x : T. f x" ~>
      Fn U "f" (Arr T T) (Fn U "x" T (App U (Var U "f") (Var U "x")))

    it "satisfies the roundtrip property" do
      quickCheck (\s -> roundtrip s <?> "Test failed for input " <> show s)

infixl 5 assertParse as ~>
assertParse :: String -> Expr -> Spec Unit
assertParse input expected =
  it ("parses " <> input <> " correctly") do
     parseExpr input `shouldEqual` Right expected

roundtrip :: RExpr -> Boolean
roundtrip (R e) =
  case parseExpr (showExprHidingUTypes e) of
    Left _ -> false
    Right e' -> e == e'

showExprHidingUTypes :: Expr -> String
showExprHidingUTypes = go
  where go :: Expr -> String
        go (Var U v) = v
        go (Var a v) = v <> " : " <> show a
        go (Fn U v U e) = "(λ" <> v <> ". " <> go e <> ")"
        go (Fn U v tv e) = "(λ" <> v <> " : " <> show tv <> ". " <> go e <> ")"
        go (Fn t v U e) = "(λ" <> v <> ". " <> go e <> ")" <> " : " <> show t
        go (Fn t v tv e) =
          "(λ" <> v <> " : " <> show tv <> ". " <> go e <> ")" <> " : " <> show t
        go (App U a b) =
          "((" <> go a <> ") (" <> go b <> "))"
        go (App t a b) =
          "((" <> go a <> ") (" <> go b <> "))" <> " : " <> show t

-- A newtype around Expr so QuickCheck can generate random values of it
-- TODO: drop the arbitrary instance and use Gen directly instead?
newtype RExpr = R Expr

instance showRExpr :: Show RExpr where
  show (R e) = showExprHidingUTypes e

instance arbitraryRExpr :: Arbitrary RExpr where
  arbitrary = Gen.oneOf $ genVar :| [genFn, genApp]

genVar :: Gen RExpr
genVar = do
  R <<< (Var T) <$> genAlphaString

genApp :: Gen RExpr
genApp = do
  R v1 <- genVar
  R v2 <- genVar
  pure $ R (App U v1 v2)

genFn :: Gen RExpr
genFn = do
  R var <- genVar
  case var of
    Var tv v -> do
      R e <- (defer \_ -> arbitrary)
      pure $ R (Fn U v tv e)
    _ -> genFn
