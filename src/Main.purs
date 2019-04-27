module Main where

import Prelude (Unit, bind, mempty, not, pure, show, ($), (&&))
import Effect (Effect)
import Effect.Console (log)
import Data.Either
import Data.String.Utils (lines)
import Data.List (List(..), filter, fromFoldable, reverse)
import Data.Traversable (traverse)
import Data.String (null)
import Data.Tuple (Tuple)
import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Map (Map)
import Data.String.Regex (regex, test)

import Expr (Expr, nf)
import Parse (parseExpr, parseAssign)

main :: Effect Unit
main = log "app loaded"

-- Like run, but doesn't distinguish between successful results and errors
run_ :: String -> String
run_ input = case run input of
               Left err -> err
               Right expr -> expr

-- Take a lambda calculus program as input, parse it, evaluate it, and return the result
run :: String -> Either String String
run input =
  case reverse $ filter (\s -> not (null s) && not (isComment s)) (fromFoldable (lines input)) of
       Cons e as -> do
          assigns <- lmap show $ traverse parseAssign as
          expr <- lmap show $ parseExpr e
          pure (show (nf (mkContext assigns) expr))
       Nil -> pure "Empty input"

mkContext :: List (Tuple String Expr) -> Map String Expr
mkContext cs = Map.fromFoldable cs

isComment :: String -> Boolean
isComment str =
  case regex "^--.+" mempty of
       Left err -> false
       Right r -> test r str
