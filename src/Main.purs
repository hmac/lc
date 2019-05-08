module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Either
import Data.Maybe
import Data.String.Utils (lines)
import Data.List (List(..), filter, fromFoldable, reverse, partition)
import Data.Traversable (traverse, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap, rmap)
import Data.Map as Map
import Data.Map (Map, lookup)
import Data.String.Regex (regex, test)

import Untyped.Parse as ParseUntyped
import Untyped as Untyped

import Simple.Parse as ParseSimple
import Simple as Simple

import SystemT.Parse as SystemT.Parse
import SystemT as SystemT

import SystemF as SystemF
import SystemF.Parse as SystemF.Parse

import Expr

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
  let ctx = foldrWithIndex (\name expr ctx -> Map.insert name (SystemF.infer mempty (map SystemF.typeOf ctx) expr) ctx) mempty defs
  main <- note "'main' not found" (lookup "main" ctx)
  let exprTypes = map SystemF.typeOf ctx
      main' = SystemF.infer mempty exprTypes main
  case SystemF.typecheck main' of
       Right unit -> pure $ show $ SystemF.nf ctx main'
       Left expr -> Left $ "Could not determine type of " <> show expr

dropComments :: String -> List String
dropComments input = filter (\s -> not (null s) && not (isComment s)) (fromFoldable (lines input))

mkContext :: forall a. List (Tuple String a) -> Map String a
mkContext cs = Map.fromFoldable cs

isComment :: String -> Boolean
isComment str =
  case regex "^--.+" mempty of
       Left err -> false
       Right r -> test r str
