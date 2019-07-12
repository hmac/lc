module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either
import Data.Maybe
import Data.String.Utils (lines)
import Data.List (List(..), filter, fromFoldable, reverse, partition, foldl)
import Data.Traversable (traverse, foldr)
import Data.Foldable (foldM)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.FunctorWithIndex (mapWithIndex)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex, class FoldableWithIndex)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Bifunctor (lmap, rmap)
import Data.Map as Map
import Data.Map (Map, delete, lookup)
import Data.String.Regex (regex, test)
import Control.Monad.Except (runExcept)

import Untyped.Parse as ParseUntyped
import Untyped as Untyped

import Simple.Parse as ParseSimple
import Simple as Simple

import SystemT.Parse as SystemT.Parse
import SystemT as SystemT

import SystemF as SystemF
import SystemF.Parse as SystemF.Parse

import HM as HM
import HM.Eval as HM.Eval
import HM.Parse as HM.Parse

import Dependent.Dependent as D
import Dependent.Parse as D.Parse

import Expr
import Pretty

main :: Effect Unit
main = log "app loaded"

runUntyped :: String -> String
runUntyped input = either identity identity $
  case reverse (dropComments input) of
       Cons e as -> do
          assigns <- lmap show $ traverse ParseUntyped.parseAssign as
          expr <- lmap show $ ParseUntyped.parseExpr e
          pure (showExpr HideTypes (Untyped.nf (mkContext assigns) expr))
       Nil -> pure "Empty input"

runSimple :: String -> String
runSimple input = either identity identity $ do
  Tuple defs types <- lmap show $ ParseSimple.parseProgram input
  main <- note "'main' not found" (lookup "main" defs)
  case Simple.typecheck (Simple.infer types defs main) of
      Right unit -> pure $ showExpr HideTypes $ Simple.nf defs main
      Left expr -> Left $ "Could not determine type of " <> show expr <> "\n\n" <> show defs <> "\n\n" <> show types

runSystemT :: String -> String
runSystemT input = either identity identity $
  case reverse (dropComments input) of
       Cons e as -> do
          assigns <- lmap show $ traverse SystemT.Parse.parseAssign as
          let ctx = foldr (\(Tuple name expr) ctx -> Map.insert name (SystemT.infer ctx expr) ctx) mempty assigns
          expr <- rmap (SystemT.infer ctx) $ lmap show $ SystemT.Parse.parseExpr e

          case SystemT.typecheck expr of
            Right unit -> pure $ show $ SystemT.nf ctx expr
            Left expr -> Left $ "Could not determine type of " <> show expr
       Nil -> pure "Empty input"

runSystemF :: String -> String
runSystemF input = either identity identity $ do
  defs <- lmap show $ SystemF.Parse.parseProgram input
  let ctx = buildContext (\ctx e -> SystemF.infer mempty (map SystemF.typeOf ctx) e) defs
  main <- note "'main' not found" (lookup "main" ctx)
  case SystemF.typecheck main of
       Right unit -> pure $ pretty $ SystemF.nf ctx main
       Left expr -> Left $ "Could not determine type of " <> pretty expr

runHM :: String -> String
runHM input = either identity identity $ do
  defs <- lmap show $ HM.Parse.parseProgram input
  main <- note "'main' not found" (lookup "main" defs)
  let expr = constructOuterLet (Map.delete "main" defs) main
  case HM.runInfer' mempty expr of
       Right t -> pure $ (pretty (HM.Eval.nf mempty expr)) <> " : " <> pretty t
       Left err -> Left err

runDependent :: String -> String
runDependent input = either identity identity $ do
  defs <- lmap show $ D.Parse.parseProgram input

  -- build up the environment from the parsed definitions
  let env = foldM (\env (Tuple name def) -> do -- this is in the Either monad
            let valContext = Map.insert name (D.nfc env.valContext def) env.valContext
            type_ <- D.runInfer env def
            let typeContext = Map.insert name type_ env.typeContext
            pure {valContext: valContext, typeContext: typeContext}
             ) {valContext: mempty, typeContext: mempty} defs
  mainExpr <- note "'main' not found" (Tuple.lookup "main" defs)
  case env of
    Left e -> pure $ e <> "\ncontext: " <> show defs
    Right env ->
      case D.runInfer env mainExpr of
        Left e -> pure $ e <> "\n\nEnv:\n " <> show env
        Right t ->
          pure $ (pretty (D.nfc env.valContext mainExpr)) <> " : " <> pretty (D.nf t)

constructOuterLet :: Map String HM.Expr -> HM.Expr -> HM.Expr
constructOuterLet defs main = foldlWithIndex (\v acc e -> HM.Let v e acc) main defs

dropComments :: String -> List String
dropComments input = filter (\s -> not (null s) && not (isComment s)) (fromFoldable (lines input))

buildContext :: forall a b k. Ord k => ((Map k b) -> a -> b) -> Map k a -> Map k b
buildContext infer defs
  = foldlWithIndex (\name ctx expr -> Map.insert name (infer ctx expr) ctx) mempty defs

mkContext :: forall a. List (Tuple String a) -> Map String a
mkContext cs = Map.fromFoldable cs

isComment :: String -> Boolean
isComment str =
  case regex "^--.+" mempty of
       Left err -> false
       Right r -> test r str
