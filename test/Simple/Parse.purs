module Test.Simple.Parse where

import Prelude
import Effect (Effect)
import Test.Assert (assertEqual)
import Data.Either (Either(..))
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Data.NonEmpty ((:|), NonEmpty)
import Control.Lazy (defer)

import Data.String.Gen (genAlphaString)

import ParseSimple (parseExpr)
import Expr
import Simple (Expr(..), Type(..))

main :: Effect Unit
main = do
  "x" ~> Var U "x"
  "y" ~> Var U "y"
  "x : A" ~> Var (T "A") "x"
  "x : B" ~> Var (T "B") "x"
  "(f : (A -> B)) (x : A)" ~> App U (Var (Arr (T "A") (T "B")) "f") (Var (T "A") "x")
  "f : A -> B x : A" ~> App U (Var (Arr (T "A") (T "B")) "f") (Var (T "A") "x")

  "f x" ~> App U (Var U "f") (Var U "x")
  "f (x : A)" ~> App U (Var U "f") (Var (T "A") "x")
  "f (x : B)" ~> App U (Var U "f") (Var (T "B") "x")

  "\\x. x" ~> Fn U "x" U (Var U "x")
  "λx. x" ~> Fn U "x" U (Var U "x")
  "\\x : A. x" ~> Fn U "x" (T "A") (Var U "x")
  "\\x : B. x" ~> Fn U "x" (T "B") (Var U "x")
  "\\f : A -> B. \\x : A. f x" ~>
    Fn U "f" (Arr (T "A") (T "B")) (Fn U "x" (T "A") (App U (Var U "f") (Var U "x")))

  quickCheck (\s -> roundtrip s <?> "Test failed for input " <> show s)

infixl 5 assertParse as ~>
assertParse :: String -> Expr -> Effect Unit
assertParse input expected =
  assertEqual { expected: Right expected, actual: parseExpr input }

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
  t <- Gen.elements typeNames
  R <<< (Var t) <$> genAlphaString

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

typeNames :: NonEmpty Array Type
typeNames = T <$> "A" :| [
  "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "V", "W", "X", "Y", "Z"
]
