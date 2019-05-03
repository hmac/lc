module SystemT where

import Prelude
import Data.Map (Map, lookup, insert, singleton)
import Data.Maybe (Maybe(..), fromMaybe)

import Simple as S

-- System T is like STLC but with a builtin Nat type

data Ann = Arr Ann Ann -- a -> b
           | Nat       -- Natural numbers
           | U         -- unknown
           | Rec       -- Rec is special-cased because it has a polymorphic type

derive instance eqAnn :: Eq Ann

instance showAnn :: Show Ann where
  show Nat = "ℕ"
  show Rec = "Rec"
  show (Arr Nat t2) = "ℕ -> " <> show t2
  show (Arr t1 t2) = "(" <> show t1 <> ") -> " <> show t2
  show U = "?"

data Expr = Fn Ann String Ann Expr -- Fn (fn type) (var name) (var type) (body)
          | Var Ann String
          | App Ann Expr Expr

-- Eq and Show instances for Expr
-- TODO: combine these with the identical ones in Simple
derive instance eqExpr :: Eq Expr
instance showExpr_ :: Show Expr where
  show = showExpr ShowUTypes
data ShowOption = ShowUTypes | HideUTypes | HideAllTypes | Raw
showExpr :: ShowOption -> Expr -> String
showExpr Raw (Var t v) = "(Var " <> show t <> " " <> v <> " )"
showExpr Raw (App t a b) = "(App " <> show t <> " " <> showExpr Raw a <> " " <> showExpr Raw b <> " )"
showExpr Raw (Fn t v tv e) = "(Fn " <> show t <> " " <> v <> " " <> show tv <> " " <> showExpr Raw e <> " )"
showExpr HideAllTypes (Var _ v) = v
showExpr HideAllTypes (Fn _ v _ e) =
  "(λ" <> v <> ". " <> showExpr HideAllTypes e <> ")"
showExpr HideAllTypes (App _ a b) =
  "((" <> showExpr HideAllTypes a <> ") (" <> showExpr HideAllTypes b <> "))"

showExpr HideUTypes (Var U v) = v
showExpr HideUTypes (Fn U v va e) =
  "(λ" <> (showExpr HideUTypes (Var va v)) <> ". " <> showExpr HideUTypes e <> ")"
showExpr HideUTypes (App U x y) =
  "((" <> showExpr HideUTypes x <> ") (" <> showExpr HideUTypes y <> "))"

showExpr HideUTypes x = showExpr ShowUTypes x

showExpr ShowUTypes (Var a v) = v <> " : " <> show a
showExpr ShowUTypes (Fn a v va e) =
  "(λ" <> v <> " : " <> show va <> ". " <> show e <> ")" <> " : " <> show a
showExpr ShowUTypes (App a x y) =
  "((" <> show x <> ") (" <> show y <> "))" <> " : " <> show a

type Context = Map String Expr

-- The idea here is to ensure you can't bind these variables to anything else
defaultContext :: Context
defaultContext
  = singleton "Zero" (Var Nat "Zero")
  <> singleton "Succ" (Var (Arr Nat Nat) "Succ")
  <> singleton "Rec" (Var U "Rec")

infer :: Context -> Expr -> Expr
-- Typing rules:

-------------
-- zero : Nat
infer ctx (Var _ "Zero") = Var Nat "Zero"

-- n : Nat
---------------
-- succ n : Nat
--
-- but it's easier to just do this:
--
--------------------
-- succ : Nat -> Nat
--
infer ctx (Var _ "Succ") = Var (Arr Nat Nat) "Succ"

-- h : Nat -> T -> T  a : T  n : Nat
------------------------------------
-- rec h a n : T
infer ctx expr@(App _ (App _ (App _ (Var _ "Rec") h) a) n)
  | (Arr Nat (Arr t1 t2)) <- getType (infer ctx h)
  , t1 == t2
  , getType (infer ctx a) == t1
  , getType (infer ctx n) == Nat
  = let typeOfh = Arr Nat (Arr t1 t1)
        typeOfrec = Arr typeOfh (Arr t1 (Arr Nat t1))
        h' = infer ctx h
        a' = infer ctx a
        n' = infer ctx n
     in App t1 (App (Arr Nat t1) (App (Arr t1 (Arr Nat t1)) (Var typeOfrec "Rec") h') a') n'
  | otherwise = expr
infer ctx (Var _ "Rec") = Var Rec "Rec"

-- the rest are identical to the STLC:

-- ctx |- t : Type  ctx |- e : t
--------------------------------  (ANN)
-- ctx |- (e : t) : t
--
-- If t is a type and e is annotated with type t, then it has type t
-- Nothing to do for this rule.
-- TODO: check that the annotated type is in the context?

-- ctx |- e : t -> t'  ctx |- e' : t
------------------------------------  (APP)
-- ctx |- ee' : t'

infer ctx (App U a b) =
  let a' = infer ctx a
      b' = infer ctx b
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

-- infer ctx (Fn U x U e) = U
infer ctx (Fn U x t e) =
  let ctx' = insert x (annotate t e) ctx
      e' = infer ctx' e
      type_ = case getType e' of
                U -> U
                t' -> Arr t t'
   in Fn type_ x t e'

-- ctx[x] = t
---------------  (VAR)
-- ctx |- x : t
infer ctx (Var U v) =
  let type_ = case lookup v ctx of
                Just e -> getType e
                Nothing -> U
   in Var type_ v

infer ctx x = x

getType :: Expr -> Ann
getType (Fn t _ _ _) = t
getType (Var t _) = t
getType (App t _ _) = t

isNat :: Expr -> Boolean
isNat e = getType e == Nat

annotate :: Ann -> Expr -> Expr
annotate a (Var U v) = Var a v
annotate a (Fn U v vt b) = Fn a v vt b
annotate a (App U x y) = App a x y
annotate _ e = e

-- System T has two special reduction rules:
--
-- rec h a 0 ⤳ a
-- rec h a (succ n) ⤳ h n (rec h a n)
--
reduce :: Context -> Expr -> Expr
reduce ctx (App t1 (App t2 (App t3 (Var trec "Rec") h) a) n)
  = case reduce ctx n of
         (Var _ "Zero") -> reduce ctx a
         (App _ (Var _ "Succ") n') -> let rec = App t2 (App t2 (App t3 (Var trec "Rec") h) a) n'
                                       in infer ctx $ App U (App U h n) rec
         n' -> App t1 (App t2 (App t3 (Var trec "Rec") h) a) n'

-- the other rules are identical to the untyped LC
reduce ctx (Var t v) = fromMaybe (Var t v) (lookup v ctx)
reduce ctx (Fn t v tv a) = Fn t v tv (reduce ctx a)
reduce ctx (App _ (Fn _ v _ a) b) = substitute v a b
reduce ctx (App t (Var tv v) b) = case lookup v ctx of
                               Just e -> App t e b
                               Nothing -> (App t (Var tv v) (reduce ctx b))
reduce ctx (App t a b) = App t (reduce ctx a) (reduce ctx b) -- do we need to reduce the argument?

-- Substitute an argument for a function parameter
-- (λx. e) a ⤳ e[a/x]
substitute :: String -> Expr -> Expr -> Expr
substitute v a b = go a
  where go (Var t v') | v' == v = b
                    | otherwise = Var t v'
        go (Fn t v' tv' e) | v' == v = Fn t v' tv' e
                     | otherwise = Fn t v' tv' (go e)
        go (App t x y) = App t (go x) (go y)
