module Dependent.Dependent where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map, lookup, insert)
import Data.List (List(..), singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.Except (Except, runExcept, throwError, withExcept)

import Pretty (class Pretty, pretty)

-- Values, Types and Kinds are all Exprs
data Expr = Var String          -- x
          | Ann Expr Expr       -- an expr annotated with a type; (e : t)
          | App Expr Expr       -- e e'
          | Lam String Expr     -- λ x. e
          | Pi String Expr Expr -- 𝚷 x : t. e
          | Type                -- *

          | Unit
          | UnitT

          -- Natural numbers
          | Nat
          | Zero
          | Succ Expr
          | NatElim Expr Expr Expr Expr -- natElim m mz ms k

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show t = genericShow t

infixr 5 space as <->
space :: String -> String -> String
space a b = a <> " " <> b

instance prettyType :: Pretty Expr where
  pretty (Var v) = v
  pretty (Ann e t) = pretty e <-> ":" <-> pretty t
  pretty (App x y) = pretty x <-> pretty y
  pretty (Lam v e) = "(λ" <> v <> "." <-> pretty e <> ")"
  pretty (Pi x t e) = "Π (" <> x <-> ":" <-> pretty t <> ")." <-> pretty e
  pretty Type = "Type"
  pretty Unit = "()"
  pretty UnitT = "Unit"
  pretty Nat = "Nat"
  pretty Zero = "Z"
  pretty (Succ e) = "S" <-> pretty e
  pretty (NatElim m mz ms k)
    = "natElim" <-> pretty m <-> pretty mz <-> pretty ms <-> pretty k

-- Context maps variable names to types
type Context = Map String Expr

-- We split the typechecking rules across two functions: infer and check
-- infer infers a type from an expression
-- check checks that an expression has the given type

-- A helper to let us annotation errors with their source
infixr 5 tagError as <?>
tagError :: forall a. Except String a -> String -> Except String a
tagError e s = withExcept (\err -> err <-> s) e

infer :: Context -> Expr -> Except String Expr

-- ANN
infer ctx (Ann e t) = do
  _ <- check ctx t Type <?> "(ANN)"
  let t' = nf t
  _ <- check ctx e t' <?> "(ANN)"
  pure t'

-- TYPE
infer ctx Type = pure Type

-- VAR
infer ctx (Var v) =
  case lookup v ctx of
    Just t -> pure t
    Nothing -> throwError $ "could not determine type of variable " <> v

-- PI
infer ctx (Pi x t e) = do
  _ <- check ctx t Type
  let t' = nf t
  let ctx' = insert x t' ctx
  _ <- check ctx' e Type <?> "(PI)"
  pure Type

-- APP
infer ctx (App e e') = do
  et <- infer ctx e
  case et of
    Pi x t t' -> do
      _ <- check ctx e' t <?> "(APP)"
      let t'' = substitute x t' e'
      pure t''
    t -> throwError $ "expected " <> pretty e <> " to be a Pi type, but was inferred to be " <> pretty t

-- Nats

infer ctx Nat = pure Type
infer ctx Zero = pure Nat
infer ctx (Succ e) = do
  _ <- check ctx e Nat <?> "(SUCC)"
  pure Nat

infer ctx (NatElim m mz ms k) = do
  _ <- check ctx m (Pi "_" Nat Type) <?> "(NATELIM)"
  let t = nf (App m Zero)
  _ <- check ctx mz t <?> "(NATELIM)"
  let t' = nf (Pi "l" Nat (Pi "_" (App m k) (App m (Succ (Var "l")))))
  _ <- check ctx ms t' <?> "(NATELIM)"
  _ <- check ctx k Nat <?> "(NATELIM)"
  pure $ nf (App m k) -- investigate this

-- Unit
infer _ UnitT = pure Type
infer _ Unit = pure UnitT

-- Fallthrough
infer ctx e = throwError $ "could not infer type of " <> pretty e

check :: Context -> Expr -> Expr -> Except String Unit

-- LAM
-- We use explicit names for lambda-abstracted variables instead of de Bruijn indices.
-- Because of this, we need to insert into the context the type of the variable
-- in the Pi type *and* and the type of the variable in the lambda abstraction.
-- I'm not 100% sure this is valid, since they should really refer to the same
-- thing, but it seems to work for now.
check ctx (Lam x e) (Pi x' t t') = do
  _ <- check ctx (nf t) Type <?> "(LAM)"
  let ctx' = insert x t ctx
  let ctx'' = insert x' t ctx'
  _ <- check ctx'' e t' <?> "(LAM)"
  pure unit

-- 𝚪 ⊢ e :↑ t
------------- (CHK)
-- 𝚪 ⊢ e :↓ t
check ctx e t
  = case runExcept (infer ctx e) of
      Left err -> throwError err
      Right t' ->
        if t' == t
          then pure unit
          else throwError $ "could not infer that " <> pretty e <>
                            " has type " <> pretty t <->
                            "(inferred type" <-> pretty t' <-> "instead)"

-- remove if unused
reducesTo :: Expr -> Expr -> Except String Unit
reducesTo from to =
  if nf from == to
  then pure unit
  else throwError $ "expected" <-> pretty from <-> "to reduce to" <-> pretty to

-- Return the normal form of the given expression, if there is one.
-- If it doesn't have a normal form, this function will hang forever.
nf :: Expr -> Expr
nf e = nfc mempty e

nfc :: Context -> Expr -> Expr
nfc ctx e =
  case reduceList ctx e of
       Cons x _ -> x
       Nil -> e

reduceList :: Context -> Expr -> List Expr
reduceList ctx expr = go (singleton expr)
  where go (Cons e es) = let e' = reduce ctx e
                          in if e' == e
                             then Cons e es
                             else go (Cons e' (Cons e es))
        go Nil = singleton expr

-- Apply beta reduction once
reduce :: Context -> Expr -> Expr
reduce c (Ann e t) = reduce c e
reduce _ Type = Type
reduce c (Pi x t e) = Pi x (reduce c t) (reduce c e)
-- reduce ctx (App _ (Var _ v) b) = case lookup v ctx of
--                                Just e -> app e b
--                                Nothing -> (app (var v) (reduce ctx b))
reduce ctx (Var v) = fromMaybe (Var v) (lookup v ctx)
reduce _ (App (Lam v a) b) = substitute v a b
reduce c (App a b) = App (reduce c a) (reduce c b)
reduce c (Lam v a) = Lam v (reduce c a)
reduce c Unit = Unit
reduce c UnitT = UnitT

reduce _ Nat = Nat
reduce _ Zero = Zero
reduce c (Succ e) = Succ (reduce c e)

reduce _ (NatElim m mz ms Zero) = mz
reduce _ (NatElim m mz ms (Succ l)) = App (App ms l) (NatElim m mz ms l)
reduce c (NatElim m mz ms k) = NatElim (reduce c m) (reduce c mz) (reduce c ms) (reduce c k)

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
        go Unit = Unit
        go UnitT = UnitT
        go Nat = Nat
        go Zero = Zero
        go (Succ e) = Succ (go e)
        go (NatElim m mz ms k) = NatElim (go m) (go mz) (go ms) (go k)
