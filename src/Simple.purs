module Simple where

import Prelude ((==), class Eq, class Show, (<>), show, Unit, unit, (&&), discard, pure)
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Expr as E

data Ann = Arr Ann Ann -- a -> b
           | T String  -- a
           | U         -- unknown

derive instance eqAnn :: Eq Ann

instance showAnn :: Show Ann where
  show (T t) = t
  show (Arr (T t1) t2) = t1 <> " -> " <> show t2
  show (Arr t1 t2) = "(" <> show t1 <> ") -> " <> show t2
  show U = "U"

data Expr = Fn Ann String Ann Expr -- Fn (fn type) (var name) (var type) (body)
          | Var Ann String
          | App Ann Expr Expr

derive instance eqExpr :: Eq Expr

instance showExpr :: Show Expr where
  show (Var a v) = v <> " : " <> show a
  show (Fn a v va e) = "(λ" <> v <> " : " <> show va <> ". " <> show e <> ")" <> " : " <> show a
  show (App a x y) = "((" <> show x <> ") (" <> show y <> "))" <> " : " <> show a

-- TODO: we'll need to be able to parse and represent type annotations
convert :: E.Expr -> Expr
convert (E.Fn v e) = Fn U v U (convert e)
convert (E.Var v) = Var U v
convert (E.App a b) = App U (convert a) (convert b)

type Context = Map String Expr

infer :: Context -> Expr -> Expr
-- Typing rules:

-- ctx |- t : Type  ctx |- e : t
--------------------------------  (ANN)
-- ctx |- (e : t) : t
--
-- If t is a type and e is annotated with type t, then it has type t
-- Nothing to do for this rule.
-- TODO: check that the annotated type is in the context?

-- ctx |- e : t -> t'  ctx |- e' : t
------------------------------------  (APP)
-- ctx |- ee' : t'

infer ctx (App U a b) =
  let a' = infer ctx a
      b' = infer ctx b
      type_ = case getType a' of
            Arr t t' ->
              case getType b' of
                U -> U
                t_ -> if t == t_ then t' else U
            _ -> U
   in App type_ a' b'

-- ctx, x : t |- e : t'
-----------------------------  (LAM)
-- ctx |- (λx -> e) : t -> t'

-- infer ctx (Fn U x U e) = U
infer ctx (Fn U x t e) =
  let ctx' = insert x (annotate t e) ctx
      e' = infer ctx' e
      type_ = case getType e' of
                U -> U
                t' -> Arr t t'
   in Fn type_ x t e'

-- ctx[x] = t
---------------  (VAR)
-- ctx |- x : t
infer ctx (Var U v) =
  let type_ = case lookup v ctx of
                Just e -> getType e
                Nothing -> U
   in Var type_ v

infer ctx x = x

-- Returns Unit if the expression typechecks.
-- If it doesn't, it returns the subexpression which is not yet typed
typecheck :: Expr -> Either Expr Unit
typecheck e@(Var U _) = Left e
typecheck e@(Fn U _ _ _) = Left e
typecheck e@(App U _ _) = Left e
typecheck e@(Var t _) = Right unit
typecheck e@(Fn t _ tv body) = do
  typecheck body
  if unify t (Arr tv (getType body))
    then pure unit
    else Left e
typecheck e@(App t f x) = do
  typecheck f
  typecheck x
  case getType f of
    Arr a b -> if unify a (getType x) && unify b t
                 then pure unit
                 else Left e
    _ -> Left f

-- Check if two types are the same
unify :: Ann -> Ann -> Boolean
unify U U = true
unify (T a) (T b) = a == b
unify (Arr a b) (Arr c d) = unify a c && unify b d
unify x y = false

getType :: Expr -> Ann
getType (Fn t _ _ _) = t
getType (Var t _) = t
getType (App t _ _) = t

annotate :: Ann -> Expr -> Expr
annotate a (Var U v) = Var a v
annotate a (Fn U v vt b) = Fn a v vt b
annotate a (App U x y) = App a x y
annotate _ e = e

-- Convert a STLC AST into an untyped AST
stripTypes :: Expr -> E.Expr
stripTypes (Var _ v) = E.Var v
stripTypes (Fn _ v _ e) = E.Fn v (stripTypes e)
stripTypes (App _ a b) = E.App (stripTypes a) (stripTypes b)
