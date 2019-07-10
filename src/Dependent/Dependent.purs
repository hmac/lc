module Dependent.Dependent where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Map (Map, lookup, insert)
import Data.List (List(..), singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Pretty (class Pretty, pretty)

-- Values, Types and Kinds are all Exprs
data Expr = Var String          -- x
          | Ann Expr Expr       -- an expr annotated with a type; (e : t)
          | App Expr Expr       -- e e'
          | Lam String Expr     -- λ x. e
          | Pi String Expr Expr -- 𝚷 x : t. e
          | Type                -- *
          | U                   -- unknown type

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show t = genericShow t

instance prettyType :: Pretty Expr where
  pretty (Var v) = v
  pretty (Ann e t) = pretty e <> " : " <> pretty t
  pretty (App x y) = pretty x <> " " <> pretty y -- TODO: make this work for nested apps
  pretty (Lam v e) = "(λ" <> v <> ". " <> pretty e <> ")"
  pretty (Pi x t e) = "Π (" <> x <> " : " <> pretty t <> "). " <> pretty e
  pretty Type = "Type"
  pretty U = "U"

-- Context maps variable names to types
type Context = Map String Expr

-- We split the typechecking rules across two functions: infer and check
-- infer infers a type from an expression
-- check checks that an expression has the given type

infer :: Context -> Expr -> {expr :: Expr, type :: Expr}

-- ANN
infer ctx (Ann e t)
  | check ctx t Type
  = let t' = nf t
     in if check ctx e t'
        then {expr: Ann e t', type: t'}
        else {expr: Ann e t', type: U}

-- TYPE
infer ctx Type = {expr: Type, type: Type}

-- VAR
infer ctx (Var v) =
  case lookup v ctx of
       Just t -> {expr: (Var v), type: t}
       Nothing -> {expr: (Var v), type: U}

-- PI
infer ctx (Pi x t e)
  | check ctx t Type
  , check (insert x (infer ctx t).expr ctx) e Type
  = {expr: Pi x t e, type: Type}

-- APP
infer ctx (App e e') =
  case (infer ctx e).type of
       Pi x t t' ->
         if check ctx e' t
         then let t'' = substitute x t' e'
               in {expr: App e e', type: t''}
         else {expr: App e e', type: U}
       _ -> {expr: App e e', type: U}

-- Fallthrough
infer ctx e = {expr: e, type: U}

check :: Context -> Expr -> Expr -> Boolean

-- LAM
check ctx (Lam x e) (Pi x' t t') =
  let ctx' = insert x t ctx
   in x == x' && check ctx' e t'

-- 𝚪 ⊢ e :↑ t
------------- (CHK)
-- 𝚪 ⊢ e :↓ t
check ctx e t = (infer ctx e).type == t


-- Return the normal form of the given expression, if there is one.
-- If it doesn't have a normal form, this function will hang forever.
nf :: Expr -> Expr
nf e =
  case reduceList e of
       Cons x _ -> x
       Nil -> e

reduceList :: Expr -> List Expr
reduceList expr = go (singleton expr)
  where go (Cons e es) = let e' = reduce e
                          in if e' == e
                             then Cons e es
                             else go (Cons e' (Cons e es))
        go Nil = singleton expr

-- Apply beta reduction once
reduce :: Expr -> Expr
reduce (Ann e t) = reduce e
reduce Type = Type
reduce U = U
reduce (Pi x t e) = Pi x (reduce t) (reduce e)
reduce (Var x) = Var x
reduce (App (Lam v a) b) = substitute v a b
reduce (App a b) = App (reduce a) (reduce b)
reduce (Lam v a) = Lam v (reduce a)

-- (λv. a) b ⤳ a[b/v]
substitute :: String -> Expr -> Expr -> Expr
substitute v a b = go a
  where go (Var v') | v' == v = b
                    | otherwise = Var v'
        go (Ann e t) = Ann (go e) (go t)
        go (App x y) = App (go x) (go y)
        go (Lam v' e) | v' == v = Lam v' e
                     | otherwise = Lam v' (go e)
        go (Pi x t e) = Pi x (go t) (go e)
        go Type = Type
        go U = U
