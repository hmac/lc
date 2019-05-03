module ParseSimple (parseExpr, parseAssign) where

import Prelude

import Text.Parsing.Parser (ParseError)
import Data.Either
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Simple (Expr, Type(..))
import Expr

import Parse as Parse

simpleConfig :: Parse.Config Type
simpleConfig = { mkNat: mkChurchNumeral
               , nullType: U
               , arrowType: Arr
               , parseType: T
               }

parseExpr :: String -> Either ParseError Expr
parseExpr = Parse.parseExpr simpleConfig

parseAssign :: String -> Either ParseError (Tuple String Expr)
parseAssign = Parse.parseAssign simpleConfig

mkChurchNumeral :: Int -> Expr
mkChurchNumeral 0 = zero
mkChurchNumeral n | n < 0 = zero -- just to avoid throwing an error
                  | otherwise = (unsafePartial succ) (mkChurchNumeral (n - 1))

zero :: Expr
zero = Fn U "f" (Arr (T "N") (T "N")) (Fn U "x" (T "N") (Var U "x"))
succ :: Partial => Expr -> Expr
succ (Fn _ _ _ (Fn _ _ _ n)) = Fn U "f" (Arr (T "N") (T "N")) (Fn U "x" (T "N") (App U (Var U "f") n))
