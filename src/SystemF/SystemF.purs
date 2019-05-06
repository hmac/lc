module SystemF where

import Prelude
import Data.Map
import Data.Maybe
import Data.List (List(..), head)
import Data.List as List

-- System F is like the STLC, but with universal quantification over types. This comes in
-- the form of the type ΠA.x (forall A. x).
data Type = TVar String
          | Arr Type Type
          | Pi String Type
          | T -- base type
          | U -- unknown

derive instance eqType :: Eq Type

instance showType :: Show Type where
  show T = "T"
  show U = "?"
  show (TVar n) = n
  show (Pi name t) = "Π" <> name <> ". " <> show t
  show (Arr T t2) = show T <> " -> " <> show t2
  show (Arr (TVar t1) t2) = t1 <> " -> " <> show t2
  show (Arr t1 t2) = "(" <> show t1 <> ") -> " <> show t2

data Expr = Var Type String
          | Ty Type                   -- a type literal, used to specialise polymorphs
          | App Type Expr Expr
          | Lam Type String Type Expr
          | Forall Type String Expr

derive instance eqExpr :: Eq Expr

instance showExpr_ :: Show Expr where
  show (Var a v) = v <> " : " <> show a
  show (Ty t) = show t
  show (Lam a v va e)
    = "(λ" <> v <> " : " <> show va <> ". " <> show e <> ")" <> " : " <> show a
  show (App a x y)
    = "((" <> show x <> ") (" <> show y <> "))" <> " : " <> show a
  show (Forall t tyname e)
    = "(forall " <> tyname <> ". " <> show e <> ") : " <> show t

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
                 Arr t1 t2 -> if t2 == typeOf b' then t2 else U
                 Pi ps pt -> case b' of
                                  Ty bt -> appType ps pt (typeOf b')
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
reduce ctx (App t a b) = App t (reduce ctx a) b
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

-- sub s t1 t2: substitute any occurrences of (TVar s) for t1 in t2
appType :: String -> Type -> Type -> Type
appType s t T = T
appType s t U = U
appType s t (TVar v) | v == s = t
                     | otherwise = TVar v
appType s t (Arr a b) = Arr (appType s t a) (appType s t b)
appType s t (Pi v a) | v == s = Pi v a
                     | otherwise = Pi v (appType s t a)
