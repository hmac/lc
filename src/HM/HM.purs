module HM where

import Prelude
import Data.Maybe
import Data.Map (Map)
import Data.Map as Map
import Pretty
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable
import Data.List (List, (:))
import Data.List as List
import Data.Traversable (traverse)
import Data.Either
import Data.Tuple
import Control.Monad.State (evalState)

import NameGen


data Type = TVar String
          | Arr Type Type
          | T -- the unit type
          | TBool

derive instance eqType :: Eq Type

derive instance genericType :: Generic Type _

instance showType :: Show Type where
  show t = genericShow t

instance prettyType :: Pretty Type where
  pretty T = "T"
  pretty TBool = "Bool"
  pretty (TVar n) = n
  pretty (Arr T t2) = pretty T <> " -> " <> pretty t2
  pretty (Arr (TVar t1) t2) = t1 <> " -> " <> pretty t2
  pretty (Arr t1 t2) = "(" <> pretty t1 <> ") -> " <> pretty t2

data Scheme = Forall (List String) Type -- TODO: change List to Set?

derive instance eqScheme :: Eq Scheme

derive instance genericScheme :: Generic Scheme _

instance showScheme :: Show Scheme where
  show t = genericShow t

instance prettyScheme :: Pretty Scheme where
  pretty (Forall vars t) = "∀ " <> fold vars <> ". " <> pretty t

data Expr = Var String
          | App Expr Expr
          | Lam String Expr
          | Let String Expr Expr
          | Unit -- the unit value
          | True
          | False

derive instance eqExpr :: Eq Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show e = genericShow e

instance prettyExpr :: Pretty Expr where
  pretty Unit = "()"
  pretty True = "True"
  pretty False = "False"
  pretty (Var v) = v
  pretty (Lam v e)
    = "(λ" <> v <> ". " <> pretty e <> ")"
  pretty (App x y)
    = "" <> pretty x <> " " <> pretty y <> ""
  pretty (Let x e1 e2)
    = "let " <> x <> " = " <> pretty e1 <> " in " <> pretty e2

newtype Subst = Subst (Map String Type)

-- Lift functions on Map to functions on Subst
delete :: String -> Subst -> Subst
delete v (Subst m) = Subst (Map.delete v m)

instance semiSubst :: Semigroup Subst where
  append :: Subst -> Subst -> Subst
  append (Subst s1) (Subst s2) = let s2' = map (apply (Subst s1)) s2
                                  in Subst (s1 <> s2')
instance monoidSubst :: Monoid Subst where
  mempty :: Subst
  mempty = Subst mempty

class Types a where
  -- get the free variables
  fv :: a -> Set String
  -- apply a substitution of type variables for types
  apply :: Subst -> a -> a

instance typesType :: Types Type where
  fv :: Type -> Set String
  fv (TVar v) = Set.singleton v
  fv (Arr a b) = fv a <> fv b
  fv _ = mempty

  apply :: Subst -> Type -> Type
  apply (Subst s) (TVar v) = fromMaybe (TVar v) (Map.lookup v s)
  apply s (Arr a b) = Arr (apply s a) (apply s b)
  apply s t = t

instance typesScheme :: Types Scheme where
  fv :: Scheme -> Set String
  fv (Forall vars t) = Set.difference (Set.fromFoldable vars) (fv t)

  apply :: Subst -> Scheme -> Scheme
  apply s (Forall vars t) =
    let s' = foldr delete s vars
     in Forall vars (apply s' t)

-- TODO: try removing this
instance typesList :: Types (List Type) where
  fv :: List Type -> Set String
  fv ts = fold (map fv ts)

  apply :: Subst -> List Type -> List Type
  apply s ts = map (apply s) ts


newtype Env = Env (Map String Scheme)

remove :: String -> Env -> Env
remove s (Env m) = Env $ Map.delete s m

lookup :: String -> Env -> Maybe Scheme
lookup v (Env m) = Map.lookup v m

singletonEnv :: String -> Scheme -> Env
singletonEnv v t = Env (Map.singleton v t)

insert :: String -> Scheme -> Env -> Env
insert v t (Env m) = Env (Map.insert v t m)

instance semiEnv :: Semigroup Env where
  append :: Env -> Env -> Env
  append (Env e1) (Env e2) = Env (e1 <> e2)

instance monoidEnv :: Monoid Env where
  mempty :: Env
  mempty = Env mempty

instance typesEnv :: Types Env where
  fv :: Env -> Set String
  fv (Env e) = fold (map fv (Map.values e))

  apply :: Subst -> Env -> Env
  apply s (Env e) = Env $ map (apply s) e

generalise :: Env -> Type -> Scheme
generalise env t =
  let freeVars = Set.difference (fv t) (fv env)
   in Forall (Set.toUnfoldable freeVars) t

newVar :: NameGen Type
newVar = TVar <$> genName

instantiate :: Scheme -> NameGen Type
instantiate (Forall vars t) = do
  newVars <- traverse (const newVar) vars
  let sub = Subst (Map.fromFoldable (List.zip vars newVars))
  pure $ apply sub t

-- TODO: maybe make this ExceptT String NameGen Subst ?
mgu :: Type -> Type -> NameGen (Either String Subst)
mgu (Arr a1 b1) (Arr a2 b2) = do
  s1 <- mgu a1 a2
  case s1 of
       Left err -> pure $ Left err
       Right s1 -> do
         s2 <- mgu (apply s1 b1) (apply s1 b2)
         case s2 of
              Left err -> pure $ Left err
              Right s2 -> pure $ Right (s1 <> s2)
mgu (TVar v) t = pure $ varBind v t
mgu t (TVar v) = pure $ varBind v t
mgu T T = pure $ Right mempty
mgu t1 t2 = pure $ Left $ "Cannot unify " <> pretty t1 <> " and " <> pretty t2

varBind :: String -> Type -> Either String Subst
varBind v t | t == TVar v = Right mempty -- don't bind a variable to itself
            | Set.member v (fv t) = Left $ "cannot bind " <> v <> " to " <> pretty t <> " because it references " <> v
            | otherwise = Right $ Subst $ Map.singleton v t

infer :: Env -> Expr -> NameGen (Either String (Tuple Subst Type))
infer env Unit = pure $ Right (Tuple mempty T)
infer env True = pure $ Right (Tuple mempty TBool)
infer env False = pure $ Right (Tuple mempty TBool)
infer env (Var v) =
  case lookup v env of
       Just sigma -> do
          t <- instantiate sigma
          pure $ Right (Tuple mempty t)
       Nothing -> pure $ Left $ "Unbound variable: " <> v
infer env (Lam x e) = do
  tv <- newVar
  let env' = env <> singletonEnv x (Forall mempty tv)
  res <- infer env' e
  case res of
       Left err -> pure $ Left err
       Right (Tuple s1 t1) -> pure $ Right $ Tuple s1 (Arr (apply s1 tv) t1)
infer env (App e1 e2) = do
  tv <- newVar
  res <- infer env e1
  case res of
       Left err -> pure $ Left err
       Right (Tuple s1 t1) -> do
          res <- infer (apply s1 env) e2
          case res of
               Left err -> pure $ Left err
               Right (Tuple s2 t2) -> do
                  res <- mgu (apply s2 t1) (Arr t2 tv)
                  case res of
                       Left err -> pure $ Left $ err <> " (" <> pretty (App e1 e2) <> ")"
                       Right s3 -> pure $ Right $ Tuple (s3 <> s2 <> s1) (apply s3 tv)
infer env (Let x e1 e2) = do
  res <- infer env e1
  case res of
       Left err -> pure $ Left err
       Right (Tuple s1 t1) -> do
          let t' = generalise (apply s1 env) t1
              env' = insert x t' env
          res <- infer (apply s1 env') e2
          case res of
               Left err -> pure $ Left err
               Right (Tuple s2 t2) -> pure $ Right $ Tuple (s1 <> s2) t2

runInfer :: Env -> Expr -> NameGen (Either String Type)
runInfer env e = do
  res <- infer env e
  case res of
       Left err -> pure $ Left err
       Right (Tuple s t) -> pure $ Right $ apply s t

runInfer' :: Env -> Expr -> Either String Type
runInfer' env e =
  let gen = Gen 0 (getVars e)
   in evalState (runInfer env e) gen

getVars :: Expr -> Set String
getVars (Var v) = Set.singleton v
getVars (Lam x e) = Set.insert x (getVars e)
getVars (App a b) = getVars a <> getVars b
getVars (Let x e1 e2) = Set.insert x (getVars e1 <> getVars e2)
getVars e = mempty

