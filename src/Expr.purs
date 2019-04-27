module Expr (Expr(..)) where

import Prelude

import Data.Map (Map, lookup)
import Data.List (List(..))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), fromMaybe)

data Expr = Fn String Expr
          | Var String
          | App Expr Expr

derive instance eqExpr :: Eq Expr

instance showExpr :: Show Expr where
  show (Var v) = v
  show (Fn v e) = "(λ" <> v <> ". " <> show e <> ")"
  show (App a b) = "(" <> show a <> " " <> show b <> ")"
