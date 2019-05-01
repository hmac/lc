module Main where

import Prelude (Unit, bind, mempty, not, pure, show, ($), (&&), (<>), map)
import Effect (Effect)
import Effect.Console (log)
import Data.Either
import Data.String.Utils (lines)
import Data.List (List(..), filter, fromFoldable, reverse)
import Data.Traversable (traverse, mapAccumL, foldl, foldr)
import Data.String (null)
import Data.Tuple (Tuple(..))
import Data.Bifunctor (lmap, rmap)
import Data.Map as Map
import Data.Map (Map)
import Data.String.Regex (regex, test)

import Parse as ParseUntyped
import Untyped as Untyped

import ParseSimple as ParseSimple
import Simple as Simple

main :: Effect Unit
main = log "app loaded"

-- Like run, but doesn't distinguish between successful results and errors
run_ :: String -> String
run_ input = case runSimple input of
               Left err -> err
               Right expr -> expr

runUntyped :: String -> Either String String
runUntyped input =
  case reverse (dropComments input) of
       Cons e as -> do
          assigns <- lmap show $ traverse ParseUntyped.parseAssign as
          expr <- lmap show $ ParseUntyped.parseExpr e
          pure (show (Untyped.nf (mkContext assigns) expr))
       Nil -> pure "Empty input"

runSimple :: String -> Either String String
runSimple input =
  case reverse (dropComments input) of
       Cons e as -> do
          assigns <- lmap show $ traverse ParseSimple.parseAssign as
          let ctx = foldr (\(Tuple name expr) ctx -> Map.insert name (Simple.infer ctx expr) ctx) mempty assigns
          expr <- rmap (Simple.infer ctx) $ lmap show $ ParseSimple.parseExpr e

          let untypedCtx = map Simple.stripTypes ctx
              untypedExpr = Simple.stripTypes expr
          case Simple.typecheck expr of
            Right unit -> pure $ show $ Untyped.nf untypedCtx untypedExpr
            Left expr -> Left $ "Could not determine type of " <> show expr
       Nil -> pure "Empty input"

dropComments :: String -> List String
dropComments input = filter (\s -> not (null s) && not (isComment s)) (fromFoldable (lines input))

typecheckAssign :: Map String Simple.Expr -> Tuple String Simple.Expr -> Either String (Tuple String Simple.Expr)
typecheckAssign ctx (Tuple name expr) =
  case Simple.typecheck (Simple.infer ctx expr) of
    Right unit -> pure (Tuple name expr)
    Left e -> Left $ "Could not determine type of " <> name <> ": " <> show e

parseAndTypecheckAssign :: String -> Either String (Tuple String Simple.Expr)
parseAndTypecheckAssign s = do
  Tuple name parsed <- lmap show $ ParseSimple.parseAssign s
  let typed = Simple.infer mempty parsed
  case Simple.typecheck typed of
    Right unit -> pure $ Tuple name typed
    Left e -> Left $ "Could not determine type of " <> name <> ": " <> show e

mkContext :: forall a. List (Tuple String a) -> Map String a
mkContext cs = Map.fromFoldable cs

isComment :: String -> Boolean
isComment str =
  case regex "^--.+" mempty of
       Left err -> false
       Right r -> test r str
