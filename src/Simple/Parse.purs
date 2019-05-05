module Simple.Parse (parseProgram, parseExpr) where

import Prelude

import Text.Parsing.Parser (ParseError)
import Data.Either
import Data.List
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Simple (Expr, Type(..))
import Expr

import Parse as Parse

simpleConfig :: Parse.Config Type
simpleConfig = { mkNat: mkChurchNumeral
               , nullType: U
               , arrowType: Arr
               , parseType: (\t -> if t == "T" then T else C t)
               }

-- Used in tests
parseExpr :: String -> Either ParseError Expr
parseExpr = Parse.parseExpr simpleConfig

parseProgram :: String -> Either ParseError (Tuple (Map String Expr) (Map String Type))
parseProgram = Parse.parseProgram simpleConfig

mkChurchNumeral :: Int -> Expr
mkChurchNumeral 0 = zero
mkChurchNumeral n | n < 0 = zero -- just to avoid throwing an error
                  | otherwise = (unsafePartial succ) (mkChurchNumeral (n - 1))

zero :: Expr
zero = Fn U "f" (Arr T T) (Fn U "x" T (Var U "x"))
succ :: Partial => Expr -> Expr
succ (Fn _ _ _ (Fn _ _ _ n)) = Fn U "f" (Arr T T) (Fn U "x" T (App U (Var U "f") n))
