module HM.Eval (nf) where

import Prelude

import Data.Map (Map, lookup)
import Data.List (List(..))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), fromMaybe)

import HM (Expr(..))

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
reduce ctx (App (Var v) b) = case lookup v ctx of
                               Just e -> App e b
                               Nothing -> (App (Var v) (reduce ctx b))
reduce ctx (App a b) = App (reduce ctx a) (reduce ctx b) -- do we need to reduce the argument?
reduce ctx Unit = Unit
reduce ctx (Let v e0 e1) = substitute v e1 e0

-- Substitute an argument for a function parameter
-- (λx. e) a ⤳ e[a/x]
substitute :: String -> Expr -> Expr -> Expr
substitute v a b = go a
  where go (Var v') | v' == v = b
                    | otherwise = Var v'
        go Unit = Unit
        go (Lam v' e) | v' == v = Lam v' e
                     | otherwise = Lam v' (go e)
        go (App x y) = App (go x) (go y)
        go (Let v' e0 e1) | v' == v = Let v' e0 e1
                          | otherwise = Let v' (go e0) (go e1)
