module Expr where

import Prelude

-- The Lambda Calculus AST
-- This type is paramterised by the type of "type annotations", which is what
-- differentiates each LC from the next
data ExprT t = Fn t String t (ExprT t)
             | Var t String
             | App t (ExprT t) (ExprT t)

derive instance eqExpr :: Eq t => Eq (ExprT t)

instance showExpr_ :: Show t => Show (ExprT t) where
  show = showExpr ShowTypes

data ShowOption = ShowTypes | HideTypes | Raw

showExpr :: forall t. Show t => ShowOption -> (ExprT t) -> String
showExpr Raw (Var t v) = "(Var " <> show t <> " " <> v <> ")"
showExpr Raw (App t a b) = "(App " <> show t <> " " <> showExpr Raw a <> " " <> showExpr Raw b <> " )"
showExpr Raw (Fn t v tv e) = "(Fn " <> show t <> " " <> v <> " " <> show tv <> " " <> showExpr Raw e <> " )"
showExpr HideTypes (Var _ v) = v
showExpr HideTypes (Fn _ v _ e) =
  "(λ" <> v <> ". " <> showExpr HideTypes e <> ")"
showExpr HideTypes (App _ a b) =
  "(" <> showExpr HideTypes a <> " " <> showExpr HideTypes b <> ")"

showExpr ShowTypes (Var a v) = v <> " : " <> show a
showExpr ShowTypes (Fn a v va e) =
  "(λ" <> v <> " : " <> show va <> ". " <> show e <> ")" <> " : " <> show a
showExpr ShowTypes (App a x y) =
  "((" <> show x <> ") (" <> show y <> "))" <> " : " <> show a
