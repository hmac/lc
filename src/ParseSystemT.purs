module ParseSystemT where

import Prelude
import Data.Either
import Text.Parsing.Parser (ParseError)
import Data.Tuple
import Parse as Parse
import Expr
import SystemT (Ann(..), Expr)

parseExpr :: String -> Either ParseError Expr
parseExpr = Parse.parseExpr config

parseAssign :: String -> Either ParseError (Tuple String Expr)
parseAssign = Parse.parseAssign config

config :: Parse.Config Ann
config = { mkNat: mkChurchNumeral
         , nullType: U
         , arrowType: Arr
         , parseType: const Nat -- the only base type is Nat
         }

mkChurchNumeral :: Int -> Expr
mkChurchNumeral 0 = zero
mkChurchNumeral n | n < 0 = zero
                  | otherwise = App Nat succ (mkChurchNumeral (n - 1))

zero :: Expr
zero = Var Nat "Zero"

succ :: Expr
succ = Var (Arr Nat Nat) "Succ"
