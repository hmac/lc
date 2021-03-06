module Simple where

import Prelude
import Data.Map (Map, lookup, insert)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import Untyped as U
import Expr

data Type = Arr Type Type -- a -> b
           | T            -- the base type
           | C String     -- a user-defined custom type (e.g. Bool : T -> T -> T)
           | U            -- unknown

derive instance eqType :: Eq Type

instance showType :: Show Type where
  show T = "T"
  show (C n) = n
  show (Arr T t2) = show T <> " -> " <> show t2
  show (Arr t1 t2) = "(" <> show t1 <> ") -> " <> show t2
  show U = "?"

type Expr = ExprT Type

type EContext = Map String Expr
type TContext = Map String Type

infer :: TContext -> EContext -> Expr -> Expr
-- Typing rules:

-- ctx |- t : Type  ctx |- e : t
--------------------------------  (ANN)
-- ctx |- (e : t) : t
-- If t is a type and e is annotated with type t, then it has type t
-- Nothing to do for this rule.
-- TODO: infer that (C "someType") is a valid type iff (C "someType") is in the context

-- ctx |- e : t -> t'  ctx |- e' : t
------------------------------------  (APP)
-- ctx |- ee' : t'

infer types ctx (App U a b) =
  let a' = infer types ctx a
      b' = infer types ctx b
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

infer types ctx (Fn U x (C t) e) =
  case lookup t types of
       Just t' -> infer types ctx (Fn U x t' e)
       Nothing -> (Fn U x (C t) e)

infer types ctx (Fn U x t e) =
  let ctx' = insert x (annotate t e) ctx
      e' = infer types ctx' e
      type_ = case getType e' of
                U -> U
                t' -> Arr t t'
   in Fn type_ x t e'

-- ctx[x] = t
---------------  (VAR)
-- ctx |- x : t
infer types ctx (Var U v) =
  let type_ = case lookup v ctx of
                Just e -> getType (infer types mempty e)
                Nothing -> U
   in Var type_ v

infer types ctx x = x

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
unify :: Type -> Type -> Boolean
unify U U = true
unify T T = true
unify (Arr a b) (Arr c d) = unify a c && unify b d
unify x y = false

getType :: Expr -> Type
getType (Fn t _ _ _) = t
getType (Var t _) = t
getType (App t _ _) = t

annotate :: Type -> Expr -> Expr
annotate a (Var U v) = Var a v
annotate a (Fn U v vt b) = Fn a v vt b
annotate a (App U x y) = App a x y
annotate _ e = e

-- Convert a STLC AST into an untyped AST
stripTypes :: Expr -> U.Expr
stripTypes (Var _ v) = U.var v
stripTypes (Fn _ v _ e) = U.fn v (stripTypes e)
stripTypes (App _ a b) = U.app (stripTypes a) (stripTypes b)

-- Return the normal form of the given expression, if there is one.
-- If it doesn't have a normal form, this function will hang forever.
-- We do this by stripping away types and deferring to the untyped LC
nf :: EContext -> Expr -> U.Expr
nf ctx expr = U.nf (map stripTypes ctx) (stripTypes expr)
