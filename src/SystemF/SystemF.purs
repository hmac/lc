module SystemF where

import Prelude
import Data.Map
import Data.Maybe

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
          | App Type Expr Expr
          | Lam Type String Type Expr
          | Forall Type String Expr

derive instance eqExpr :: Eq Expr

instance showExpr_ :: Show Expr where
  show (Var a v) = v <> " : " <> show a
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

annotate :: Expr -> Type -> Expr
annotate (Var _ v) t = Var t v
annotate (App _ a b) t = App t a b
annotate (Lam _ v vt e) t = Lam t v vt e
annotate (Forall _ ts e) t = Forall t ts e
