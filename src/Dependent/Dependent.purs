module Dependent.Dependent where

import Prelude
import Data.Maybe (maybe, Maybe(..))
import Data.Map (Map, lookup, insert)
import Data.Map as Map
import Data.List (List(..), singleton)
import Data.Generic.Rep
import Data.Generic.Rep.Show

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

-- TODO: this could probably be List (Tuple Expr Expr)
-- Context maps variable names to types
type Context = Map String Expr

infer :: Context -> Expr -> {expr :: Expr, type :: Expr}

infer ctx (Ann e t)
  | (infer ctx t).type == Type =
  let t' = nf t
   in {expr: Ann e t', type: t'}

infer ctx Type = {expr: Type, type: Type}

infer ctx (Var v) =
  case lookup v ctx of
       Just t -> {expr: (Var v), type: t}
       Nothing -> {expr: (Var v), type: U}

infer ctx (Pi x t e)
  | (infer ctx t).type == Type
  , (infer (insert x (infer ctx t).expr ctx) e).type == Type
  = {expr: Pi x t e, type: Type}

{-- infer ctx (App (Pi x t t') e') = --}
{--   if (infer ctx e').type == t --}
{--   then let t'' = substitute x t' e' --}
{--         in {expr: App (Pi x t t') e', type: t''} --}
{--   else {expr: App (Pi x t t') e', type: U} --}

infer ctx (App e e') =
  case (infer ctx e).type of
       Pi x t t' ->
         if (infer ctx e').type == t
         then let t'' = substitute x t' e'
               in {expr: App e e', type: t''}
         else {expr: App e e', type: U}
       _ -> {expr: App e e', type: U}


-- I'm unsure what the type of x should be here
infer ctx (Lam x e) =
  let ctx' = insert x U ctx
      t' = (infer ctx' e).type
   in {expr: Lam x e, type: Pi x U t'}

-- 𝚪 ⊢ e :↑ t
------------- (CHK)
-- 𝚪 ⊢ e :↓ t
-- TODO


-- Fallthrough
infer ctx e = {expr: e, type: U}

-- Split the typechecking rules across two functions: infer and check
-- infer :: Context -> Expr -> Expr
-- check :: Context -> Expr -> Expr -> Bool
-- infer infers a type from an expression
-- check checks that an expression has the given type

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
