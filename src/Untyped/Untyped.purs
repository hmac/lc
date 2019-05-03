module Untyped (nf, Expr, var, fn, app) where

import Prelude

import Data.Map (Map, lookup)
import Data.List (List(..))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe (Maybe(..), fromMaybe)

import Expr

type Expr = ExprT Unit

-- Convenience functions for constructing terms
var :: String -> Expr
var = Var unit

fn :: String -> Expr -> Expr
fn v e = Fn unit v unit e

app :: Expr -> Expr -> Expr
app = App unit

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
reduce ctx (Var _ v) = fromMaybe (var v) (lookup v ctx)
reduce ctx (Fn _ v _ a) = fn v (reduce ctx a)
reduce ctx (App _ (Fn _ v _ a) b) = substitute v a b
reduce ctx (App _ (Var _ v) b) = case lookup v ctx of
                               Just e -> app e b
                               Nothing -> (app (var v) (reduce ctx b))
reduce ctx (App _ a b) = app (reduce ctx a) (reduce ctx b) -- do we need to reduce the argument?

-- Substitute an argument for a function parameter
-- (λx. e) a ⤳ e[a/x]
substitute :: String -> Expr -> Expr -> Expr
substitute v a b = go a
  where go (Var _ v') | v' == v = b
                    | otherwise = var v'
        go (Fn _ v' _ e) | v' == v = fn v' e
                     | otherwise = fn v' (go e)
        go (App _ x y) = app (go x) (go y)
