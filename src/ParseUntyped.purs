module ParseUntyped (parseExpr, parseAssign) where

-- This is the first parser I wrote for this project.
-- It's a bit simpler than the one in Parse because it doesn't have to worry about type
-- annotations.
-- Instead of trying to merge the two together, and deal with optional annotations, I've
-- just left it alone.

import Prelude (bind, otherwise, pure, ($), (-), (/=), (<), (<$>), (<<<))

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy1)
import Text.Parsing.Parser.String (string, satisfy, oneOf)

import Control.Lazy (fix)
import Data.Either
import Data.Array (some)
import Control.Alt ((<|>))
import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), foldl)
import Global (readInt)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Expr
import Untyped (Expr, var, app, fn)

type P = Parser String

parseExpr :: String -> Either ParseError Expr
parseExpr input = runParser input expr

parseAssign :: String -> Either ParseError (Tuple String Expr)
parseAssign input = runParser input assign

assign :: Parser String (Tuple String Expr)
assign = do
  name <- ident
  _ <- string " = "
  e <- expr
  pure $ Tuple name e

expr :: Parser String Expr
expr = fix $ \p -> expr' p
-- expr = fix $ \p -> (func <|> try (app p) <|> aexpr p)

expr' :: Parser String Expr -> Parser String Expr
expr' p = application p <|> aexpr p

aexpr :: Parser String Expr -> Parser String Expr
aexpr p = parens expr <|> number <|> func p <|> variable

number :: Parser String Expr
number = do
  n <- readInt 10 <<< fromCharArray <$> some (oneOf digits)
  case fromNumber n of
    Just i -> pure $ mkChurchNumeral i
    Nothing -> fail "cannot parse number"

mkChurchNumeral :: Int -> Expr
mkChurchNumeral 0 = zero
mkChurchNumeral n | n < 0 = zero -- just to avoid throwing an error
                  | otherwise = (unsafePartial succ) (mkChurchNumeral (n - 1))

zero :: Expr
zero = fn "f" (fn "x" (var "x"))
succ :: Partial => Expr -> Expr
succ (Fn _ _ _ (Fn _ _ _ n)) = fn "f" (fn "x" (app (var "f") n))

func :: Parser String Expr -> Parser String Expr
func p = do
  _ <- string "\\"
  v <- some (satisfy (_ /= '.'))
  _ <- string ". "
  e <- expr' p
  pure $ fn (fromCharArray v) e

variable :: Parser String Expr
variable = var <$> ident

ident :: Parser String String
ident = fromCharArray <$> some (oneOf alphas)

application :: Parser String Expr -> Parser String Expr
application p = do
  _ <- pure 1
  apps <- sepBy1 (aexpr p) (string " ")
  case apps of
    Cons e es -> pure $ foldl app e es
    Nil -> fail $ "Expected at least one application"

parens :: Parser String Expr -> Parser String Expr
parens p = between (string "(") (string ")") p

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

alphas :: Array Char
alphas = [
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  ]
