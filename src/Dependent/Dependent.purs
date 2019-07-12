module Dependent.Dependent where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map, lookup, insert)
import Data.List (List(..), singleton)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Reader (Reader, runReader, ask, local)
import Data.Foldable (foldr)

import Pretty (class Pretty, pretty)

-- Values, Types and Kinds are all Exprs
data Expr = Var String                  -- x
          | Ann Expr Expr               -- an expr annotated with a type; (e : t)
          | App Expr Expr               -- e e'
          | Lam String Expr             -- λ x. e
          | Pi String Expr Expr         -- 𝚷 x : t. e
          | Type                        -- *

          | Unit
          | UnitT

          -- Natural numbers
          | Nat
          | Zero
          | Succ Expr
          | NatElim Expr Expr Expr Expr -- natElim m mz ms k

          -- Vectors
          | Vec Expr Expr
          | VNil Expr                   -- Nil : 𝚷 (a : Type). Vec a Zero
          | VCons Expr Expr Expr Expr   -- Cons : 𝚷 (a : Type). 𝚷 (k : Nat).
                                        --        a -> Vec a k -> Vec a (Succ k)
          | VecElim Expr Expr Expr Expr Expr Expr
          -- vecElim : 𝚷(a : Type).
          --           𝚷(m : 𝚷(k : Nat). 𝚷(_ : Vec a k). Type).
          --           m Zero (VNil a) ->
          --           (𝚷(l : Nat). 𝚷(x : a). 𝚷(xs : Vec a l).
          --            m l xs ->
          --            m (Succ l) (VCons a l x xs)).
          --           𝚷(k : Nat). 𝚷(xs : Vec a k).
          --           m k xs

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show t = genericShow t

infixr 5 space as <->
space :: String -> String -> String
space a b = a <> " " <> b

parens :: String -> String
parens x = "(" <> x <> ")"

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
  pretty (NatElim m mz ms k) = foldr (<->) "natElim" (map pretty [m, mz, ms, k])
  pretty (Vec a k) = "Vec" <-> pretty a <-> parens (pretty k)
  pretty (VNil a) = "[]"
  pretty (VCons a k x xs) = pretty x <-> "::" <-> pretty xs
  pretty (VecElim a m mz ms k xs) = foldr (<->) "vecElim" (map pretty [a, m, mz, ms, k, xs])

-- Context maps variable names to types
type Context = Map String Expr

-- We split the typechecking rules across two functions: infer and check
-- infer infers a type from an expression
-- check checks that an expression has the given type

-- A helper to let us annotation errors with their source
infixr 5 tagError as <?>
tagError :: forall m a. Monad m => ExceptT String m a -> String -> ExceptT String m a
tagError e s = withExceptT (\err -> err <-> s) e

runInfer :: Env -> Expr -> Either String Expr
runInfer env expr = runReader (runExceptT (infer expr)) env

type Env = {typeContext :: Context, valContext :: Context}
infer :: Expr -> ExceptT String (Reader Env) Expr

-- ANN
infer (Ann e t) = do
  _ <- check t Type <?> "(ANN)"
  {valContext: ex} <- ask
  let t' = nfc ex t
  _ <- check e t' <?> "(ANN)"
  pure t'

-- TYPE
infer Type = pure Type

-- VAR
infer (Var v) = do
  {typeContext: ctx} <- ask
  case lookup v ctx of
    Just t -> pure t
    Nothing -> throwError $ "could not determine type of variable " <> v <> "\n" <> show ctx

-- PI
infer (Pi x t e) = do
  _ <- check t Type
  {valContext: ex, typeContext: ctx} <- ask
  let t' = nfc ex t
  let ctx' = insert x t' ctx
  let env' = {valContext: ex, typeContext: ctx'}
  _ <- local (const env') $ check e Type <?> "(PI)"
  pure Type

-- APP
infer (App e e') = do
  et <- infer e
  case et of
    Pi x t t' -> do
      _ <- check e' t <?> "(APP)"
      let t'' = substitute x t' e'
      pure t''
    t -> throwError $ "expected " <> pretty e <> " to be a Pi type, but was inferred to be " <> pretty t

-- Nats

infer Nat = pure Type
infer Zero = pure Nat
infer (Succ e) = do
  _ <- check e Nat <?> "(SUCC)"
  pure Nat

infer (NatElim m mz ms k) = do
  _ <- check m (Pi "_" Nat Type) <?> "(NATELIM)"
  {valContext: ex} <- ask
  let t = nfc ex (App m Zero)
  _ <- check mz t <?> "(NATELIM)"
  let t' = nfc ex (Pi "l" Nat (Pi "_" (App m k) (App m (Succ (Var "l")))))
  _ <- check ms t' <?> "(NATELIM)"
  _ <- check k Nat <?> "(NATELIM)"
  pure $ nfc ex (App m k)

-- Vecs

infer (Vec a k) = pure Type
infer (VNil a) = do
  {valContext: ex} <- ask
  _ <- check a Type
  pure (Vec (nfc ex a) Zero)
infer (VCons a k x xs) = do
  {valContext: ex} <- ask
  _ <- check a Type <?> "(VCONS)"
  _ <- check k Nat <?> "(VCONS)"
  let a' = nfc ex a
      k' = nfc ex k
  _ <- check x a' <?> "(VCONS)"
  _ <- check xs (Vec a' k') <?> "(VCONS)"
  pure $ Vec a' (Succ k')
infer (VecElim a m mz ms k xs) = do
  {valContext: ex} <- ask
  _ <- check a Type
  let a' = nfc ex a
  _ <- check m $ Pi "k" Nat (Pi "_" (Vec a' (Var "k")) Type)
  let m' = nfc ex m
  _ <- check mz $ nfc ex $ App (App m' Zero) (VNil a')
  _ <- check ms $
    Pi "l" Nat
      (Pi "x" a'
      (Pi "xs" (Vec a' (Var "l"))
      (Pi "_" (App (App m' (Var "l")) (Var "xs"))
              (App (App m' (Succ (Var "l")))
                   (VCons a' (Var "l") (Var "x") (Var "xs"))))))
  _ <- check k Nat
  let k' = nfc ex k
  _ <- check xs (Vec a k')
  let xs' = nfc ex xs
  pure $ App (App m' k') xs'

-- Unit
infer UnitT = pure Type
infer Unit = pure UnitT

-- Fallthrough
infer e = throwError $ "could not infer type of " <> pretty e

check :: Expr -> Expr -> ExceptT String (Reader Env) Unit

-- LAM
-- We use explicit names for lambda-abstracted variables instead of de Bruijn indices.
-- Because of this, we need to insert into the context the type of the variable
-- in the Pi type *and* and the type of the variable in the lambda abstraction.
-- I'm not 100% sure this is valid, since they should really refer to the same
-- thing, but it seems to work for now.
check (Lam x e) (Pi x' t t') = do
  {valContext: ex, typeContext: ctx} <- ask
  _ <- check (nfc ex t) Type <?> "(LAM)"
  let ctx' = insert x t ctx
  let ctx'' = insert x' t ctx'
  let env' = {valContext: ex, typeContext: ctx''}
  _ <- local (const env') $ check e t' <?> "(LAM)"
  pure unit

-- 𝚪 ⊢ e :↑ t
------------- (CHK)
-- 𝚪 ⊢ e :↓ t
check e t = do
  {valContext: ex} <- ask
  t' <- infer e
  let t'n = nfc ex t'
      tn = nfc ex t
  if t'n == tn
    then pure unit
    else throwError $ "could not infer that " <> pretty (nfc ex e) <>
                      " has type " <> pretty tn <->
                      "(inferred type" <-> pretty t'n <-> "instead)"

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
reduce c (Pi x t e) = Pi x (reduce c t) (reduce c e)
reduce ctx (Var v) = fromMaybe (Var v) (lookup v ctx)
reduce _ (App (Lam v a) b) = substitute v a b
reduce c (App a b) = App (reduce c a) (reduce c b)
reduce c (Lam v a) = Lam v (reduce c a)

reduce c (Succ e) = Succ (reduce c e)

reduce _ (NatElim m mz ms Zero) = mz
reduce _ (NatElim m mz ms (Succ l)) = App (App ms l) (NatElim m mz ms l)
reduce c (NatElim m mz ms k) = NatElim (reduce c m) (reduce c mz) (reduce c ms) (reduce c k)

reduce c (Vec a k) = Vec (reduce c a) (reduce c k)
reduce c (VNil a) = VNil (reduce c a)
reduce c (VCons a k x xs) = VCons (reduce c a) (reduce c k) (reduce c x) (reduce c xs)

reduce c (VecElim a m mz ms Zero xs) = mz
reduce c (VecElim a m mz ms (Succ k) (VCons _ _ x xs))
  = App (App (App (App ms k) x) xs) (VecElim a m mz ms k xs)
reduce c (VecElim a m mz ms k xs) = VecElim (reduce c a) (reduce c m) (reduce c mz) (reduce c ms) (reduce c k) (reduce c xs)

-- Fall-through for nullary constructors
reduce c e = e

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
        go (Succ e) = Succ (go e)
        go (NatElim m mz ms k) = NatElim (go m) (go mz) (go ms) (go k)
        go (Vec t k) = Vec (go t) (go k)
        go (VNil t) = VNil (go t)
        go (VCons t k x xs) = VCons (go t) (go k) (go x) (go xs)
        go (VecElim t m mz ms k xs) = VecElim (go t) (go m) (go mz) (go ms) (go k) (go xs)
        -- Fallthrough for nullary constructors
        go e = e
