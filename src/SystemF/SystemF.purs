module SystemF where

import Prelude
import Data.Map
import Data.Maybe
import Data.List (List(..), head)
import Data.List as List
import Data.Either
import Pretty
import Data.Generic.Rep
import Data.Generic.Rep.Show

-- System F is like the STLC, but with universal quantification over types.
-- We have two new terms:
--
-- Term                Type
-----------------------------
-- forall X. (e : t)   ΠX. t
-- T                   TVar T
--
-- forall can only be applied to type literals, e.g.
-- (forall X. \x : X. x) T ⤳ \x : T. x

data Type = TVar String
          | Arr Type Type
          | Pi String Type
          | T -- base type
          | U -- unknown

derive instance eqType :: Eq Type

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show t = genericShow t

instance prettyType :: Pretty Type where
  pretty T = "T"
  pretty U = "?"
  pretty (TVar n) = n
  pretty (Pi name t) = "Π" <> name <> ". " <> pretty t
  pretty (Arr T t2) = pretty T <> " -> " <> pretty t2
  pretty (Arr (TVar t1) t2) = t1 <> " -> " <> pretty t2
  pretty (Arr t1 t2) = "(" <> pretty t1 <> ") -> " <> pretty t2

data Expr = Var Type String
          | Ty Type         -- a type literal, used to specialise polymorphs
          | App Type Expr Expr
          | Lam Type String Type Expr
          | Forall Type String Expr

derive instance eqExpr :: Eq Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show e = genericShow e

instance prettyExpr :: Pretty Expr where
  pretty (Var a v) = v <> " : " <> pretty a
  pretty (Ty t) = pretty t
  pretty (Lam a v va e)
    = "(λ" <> v <> " : " <> pretty va <> ". " <> pretty e <> ")" <> " : " <> pretty a
  pretty (App a x y)
    = "((" <> pretty x <> ") (" <> pretty y <> "))" <> " : " <> pretty a
  pretty (Forall t tyname e)
    = "(Λ" <> tyname <> ". " <> pretty e <> ") : " <> pretty t

-- A mapping from variables names to variables types
type EContext = Map String Type
-- A mapping from type variable names to types
type TContext = Map String Type

infer :: TContext -> EContext -> Expr -> Expr
infer ts es (Var U v) = Var (fromMaybe U (lookup v es)) v
infer ts es (App U a b)
  = let a' = infer ts es a
        b' = infer ts es b
        t = case typeOf a' of
                 Arr t1 t2 -> if t1 == typeOf b' then t2 else U
                 Pi ps pt -> case b' of
                                  Ty bt -> appType ps bt pt
                                  _ -> U
                 _ -> U
     in App t a' b'
infer ts es (Lam U v vt e)
-- if a type variable is used, it must be present in the context
  = if checkType ts vt
    then
      let e' = infer ts (insert v vt es) e
          t = Arr vt (typeOf e')
       in Lam t v vt e'
    else
      Lam U v vt e
infer ts es (Forall U s e)
  = let e' = infer (insert s (TVar s) ts) es e
        t = Pi s (typeOf e')
     in Forall t s e'
infer ts es e = e

checkType :: TContext -> Type -> Boolean
checkType ts T = true
checkType ts U = false
checkType ts (TVar v) = member v ts
checkType ts (Arr t1 t2) = checkType ts t1 && checkType ts t2
checkType ts (Pi s t) = checkType (insert s (TVar s) ts) t

typeOf :: Expr -> Type
typeOf (Var t _) = t
typeOf (App t _ _) = t
typeOf (Lam t _ _ _) = t
typeOf (Forall t _ _) = t
typeOf (Ty t) = t

-- reduction follows the same rules as the untyped LC, with the addition of
-- application of types to forall terms

-- A mapping from variable names to expressions
type RContext = Map String Expr

nf :: RContext -> Expr -> Expr
nf ctx e = fromMaybe e (head (reduceList ctx e))

reduceList :: RContext -> Expr -> List Expr
reduceList ctx expr = go (List.singleton expr)
  where go (Cons e es) = let e' = reduce ctx e
                          in if e' == e
                             then Cons e es
                             else go (Cons e' (Cons e es))
        go Nil = List.singleton expr

reduce :: RContext -> Expr -> Expr
reduce ctx (Ty t) = Ty t
reduce ctx (Var t v) = fromMaybe (Var t v) (lookup v ctx)
reduce ctx (App t (Lam tf v tv a) b) = subVar v a b
reduce ctx (App t (Forall tf s a) (Ty b)) = subType s b a
reduce ctx (App t a b) = App t (reduce ctx a) (reduce ctx b)
reduce ctx (Lam t v tv e) = Lam t v tv (reduce ctx e)
reduce ctx (Forall t s e) = Forall t s (reduce ctx e)


-- Substitute an argument for a formal parameter
-- (λx. e) a ⤳ e[a/x]
subVar :: String -> Expr -> Expr -> Expr
subVar v a b = go a
  where go (Var t v') | v' == v = b
                    | otherwise = Var t v'
        go (Lam t v' tv e) | v' == v = Lam t v' tv e
                     | otherwise = Lam t v' tv (go e)
        go (App t x y) = App t (go x) (go y)
        go (Forall t s e) = Forall t s (go e)
        go (Ty t) = Ty t

-- Substitute a type for a type variable
-- (ΛX. e) A ⤳ e[A/X]
-- subType s t e: substitute any occurrences of (TVar s) for t in e
subType :: String -> Type -> Expr -> Expr
subType s t (Ty tt) = Ty (appType s t tt)
subType s t (Var vt v) = Var (appType s t vt) v
subType s t (App at a b) = App (appType s t at) (subType s t a) (subType s t b)
subType s t (Lam lt v vt e) = Lam (appType s t lt) v (appType s t vt) (subType s t e)
subType s t (Forall ft fs e) | s == fs = Forall (appType s t ft) fs e
                             | otherwise = Forall (appType s t ft) fs (subType s t e)

-- appType s t1 t2: substitute any occurrences of (TVar s) for t1 in t2
appType :: String -> Type -> Type -> Type
appType s t T = T
appType s t U = U
appType s t (TVar v) | v == s = t
                     | otherwise = TVar v
appType s t (Arr a b) = Arr (appType s t a) (appType s t b)
appType s t (Pi v a) | v == s = Pi v a
                     | otherwise = Pi v (appType s t a)

-- Returns Unit if the expression typechecks.
-- If it doesn't, it returns the subexpression which is not yet typed
-- TODO: check if we can simplify this, it was lifted from Simple
typecheck :: Expr -> Either Expr Unit
typecheck (Ty _) = Right unit
typecheck e@(Var U _) = Left e
typecheck e@(Lam U _ _ _) = Left e
typecheck e@(Forall U _ _) = Left e
typecheck e@(App U _ _) = Left e
typecheck e@(Var t _) = Right unit
typecheck e@(Lam t _ tv body) = do
  typecheck body
  if unify t (Arr tv (typeOf body))
    then pure unit
    else Left e
typecheck e@(Forall t _ _) = Right unit
typecheck e@(App t f x) = do
  typecheck f
  typecheck x
  case typeOf f of
       Arr a b -> if unify a (typeOf x) && unify b t
                  then pure unit
                  else Left e
       Pi s t -> Right unit
       _ -> Left f

-- Check if two types are the same
unify :: Type -> Type -> Boolean
unify U U = true
unify T T = true
unify (Arr a b) (Arr c d) = unify a c && unify b d
unify (TVar a) (TVar b) = a == b
unify x y = false
