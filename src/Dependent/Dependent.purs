module Dependent.Dependent where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Map (Map, lookup, insert)
import Data.List (List(..), singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.Except

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

infer :: Context -> Expr -> Except String {expr :: Expr, type :: Expr}

-- ANN
infer ctx (Ann e t) = do
  _ <- check ctx t Type
  let t' = nf t
  _ <- check ctx e t'
  pure {expr: Ann e t', type: t'}

-- TYPE
infer ctx Type = pure {expr: Type, type: Type}

-- VAR
infer ctx (Var v) =
  case lookup v ctx of
    Just t -> pure {expr: (Var v), type: t}
    Nothing -> throwError $ "could not determine type of variable " <> v

-- PI
infer ctx (Pi x t e) = do
  _ <- check ctx t Type
  let t' = nf t
  let ctx' = insert x t' ctx
  _ <- check ctx' e Type
  pure {expr: Pi x t e, type: Type}

-- APP
infer ctx (App e e') = do
  et <- infer ctx e
  case et.type of
    Pi x t t' -> do
      _ <- check ctx e' t
      let t'' = substitute x t' e'
      pure {expr: App e e', type: t''}
    t -> throwError $ "expected " <> pretty e <> " to be a Pi type, but was inferred to be " <> pretty t

-- Fallthrough
infer ctx e = throwError $ "could not infer type of " <> pretty e

-- TODO: make into an Except
check :: Context -> Expr -> Expr -> Except String Unit

-- LAM
check ctx (Lam x e) (Pi x' t t')
  = do
      let ctx' = insert x t ctx
      _ <- check ctx' e t'
      pure unit

-- 𝚪 ⊢ e :↑ t
------------- (CHK)
-- 𝚪 ⊢ e :↓ t
check ctx e t
  = case runExcept (infer ctx e) of
      Left err -> throwError err
      Right {type: t'} ->
        if t' == t
          then pure unit
          else throwError $ "could not infer that " <> pretty e <> " has type " <> pretty t


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
