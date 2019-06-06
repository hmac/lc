module HM.Eval (nf, runPrim, reduce) where

import Prelude

import Data.Tuple
import Data.Map as Map
import Data.Map (Map, lookup, member)
import Data.List (List(..), singleton, length, head, last, snoc)
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), fromMaybe)

import HM (Expr(..), Type(..), builtins)

type Context = Map String Expr

-- Return the normal form of the given expression, if there is one.
-- If it doesn't have a normal form, this function will hang forever.
nf :: Context -> Expr -> Expr
nf ctx e = NonEmpty.head (reduceList ctx e)

-- Apply beta reduction repeatedly until we reach normal form
-- This will loop indefinitely if the expression has no normal form
-- TODO: add a maximum reduction limit to catch this
reduceList :: Context -> Expr -> NonEmpty List Expr
reduceList ctx expr = go (NonEmpty.singleton expr)
  where go :: NonEmpty List Expr -> NonEmpty List Expr
        go (NonEmpty e es) = let e' = reduce ctx e
                             in if e' == e
                                then e :| es
                                else go (e' :| (Cons e es))

-- Apply beta reduction once
reduce :: Context -> Expr -> Expr
reduce ctx (Var v) = fromMaybe (Var v) (lookup v ctx)
reduce ctx (Lam v a) = Lam v (reduce ctx a)
reduce ctx (App (Lam v a) b) = substitute v a b
reduce ctx (App (Var v) b) | Just e <- lookup v ctx = App e b
                           | Just prim <- lookup v builtins = App prim b
                           | otherwise = App (Var v) (reduce ctx b)
reduce ctx (App (Prim n arity t args) b)
  | arity > length args = Prim n arity t (snoc args (nf ctx b)) -- heads up, we use nf here
  | otherwise = App (reduce ctx (Prim n arity t args)) b
reduce ctx (App a b) = App (reduce ctx a) (reduce ctx b)
reduce ctx (Let v e0 e1) = substitute v e1 e0
reduce ctx (Prim name arity ty args)
  | arity == length args = runPrim name arity ty args
  | otherwise = Prim name arity ty args
reduce ctx e = e -- Unit, True, False

-- Substitute an argument for a function parameter
-- (λx. e) a ⤳ e[a/x]
substitute :: String -> Expr -> Expr -> Expr
substitute v a b = go a
  where go (Var v') | v' == v = b
                    | otherwise = Var v'
        go (Lam v' e) | v' == v = Lam v' e
                     | otherwise = Lam v' (go e)
        go (App x y) = App (go x) (go y)
        go (Let v' e0 e1) | v' == v = Let v' e0 e1
                          | otherwise = Let v' (go e0) (go e1)
        go e = e -- Unit, True, False, integer, primitive

runPrim :: String -> Int -> Type -> List Expr -> Expr
runPrim "add" arity (Arr TInt (Arr TInt TInt)) args =
  let default = Prim "mul" arity (Arr TInt (Arr TInt TInt)) args
   in case head args of
       Just (IntLit a) -> case last args of
                      Just (IntLit b) -> IntLit (a + b)
                      _ -> default
       _ -> default
runPrim "mul" arity (Arr TInt (Arr TInt TInt)) args =
  let default = Prim "mul" arity (Arr TInt (Arr TInt TInt)) args
   in case head args of
       Just (IntLit a) -> case last args of
                            Just (IntLit b) -> IntLit (a * b)
                            _ -> default
       _ -> default
runPrim n a t args = Prim n a t args
