module Simple where

import Prelude ((==), class Eq, class Show, (<>), show)
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(..))

import Expr as E

data Ann = Arr Ann Ann
           | T String
           | U -- unknown

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

-- TODO: make this more efficient by changing infer' :: Context -> Expr -> Expr
infer :: Context -> Expr -> Expr
infer ctx e@(Var U v) = Var (infer' ctx e) v
infer ctx e@(Fn U v tv body) =
  let ctx' = insert v (annotate tv body) ctx
      body' = infer ctx' body
   in Fn (infer' ctx (Fn U v tv body')) v tv body'
infer ctx e@(App U a b) =
  let a' = infer ctx a
      b' = infer ctx b
   in App (infer' ctx (App U a' b')) a' b'
infer ctx e = e

infer' :: Context -> Expr -> Ann
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

infer' ctx (App U a b) =
  case getType a of
       Arr t t' ->
         case getType b of
              U -> U
              t_ -> if t == t_ then t' else U
       _ -> U

-- ctx, x : t |- e : t'
-----------------------------  (LAM)
-- ctx |- (λx -> e) : t -> t'

-- infer' ctx (Fn U x U e) = U
infer' ctx (Fn U x t e) =
  case getType e of
       U -> U
       t' -> Arr t t'

-- ctx[x] = t
---------------  (VAR)
-- ctx |- x : t
infer' ctx (Var U v) =
  case lookup v ctx of
       Just e -> getType e
       Nothing -> U

infer' ctx x = U

getType :: Expr -> Ann
getType (Fn t _ _ _) = t
getType (Var t _) = t
getType (App t _ _) = t

annotate :: Ann -> Expr -> Expr
annotate a (Var U v) = Var a v
annotate a (Fn U v vt b) = Fn a v vt b
annotate a (App U x y) = App a x y
annotate _ e = e
