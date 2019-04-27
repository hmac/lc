module ParseSimple (parseExpr, parseAssign) where

import Prelude (bind, otherwise, pure, ($), (-), (<), (<$>), (<<<), (<>), discard, (*>), (==))

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy1, option)
import Text.Parsing.Parser.String (string, oneOf, skipSpaces)

import Control.Lazy (fix)
import Data.Either
import Data.Array (some, cons, many)
import Control.Alt ((<|>))
import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), foldl, foldr, reverse)
import Global (readInt)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

import Simple (Expr(..), Ann(..))

type P = Parser String

parseAnn :: String -> Either ParseError Ann
parseAnn input = runParser input annotation

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

expr' :: Parser String Expr -> Parser String Expr
expr' p = app p <|> aexpr p

aexpr :: Parser String Expr -> Parser String Expr
aexpr p = parens expr <|> number <|> func p <|> var

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
zero = Fn U "f" (Arr (T "N") (T "N")) (Fn U "x" (T "N") (Var U "x"))
succ :: Partial => Expr -> Expr
succ (Fn _ _ _ (Fn _ _ _ n)) = Fn U "f" (Arr (T "N") (T "N")) (Fn U "x" (T "N") (App U (Var U "f") n))

func :: Parser String Expr -> Parser String Expr
func p = do
  _ <- (string "\\" <|> string "λ")
  v <- ident
  skipSpaces
  t <- option U (string ":" *> skipSpaces *> annotation)
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Fn U v t e

-- Parsing type annotations
annotation :: Parser String Ann
annotation = fix $ \p -> annExpr p
annExpr :: Parser String Ann -> Parser String Ann
annExpr p = annArrow p <|> annExpr' p
annArrow :: Parser String Ann -> Parser String Ann
annArrow p = do
  apps <- sepBy1 (annExpr' p) (string " -> ")
  case reverse apps of
    Nil -> fail $ "Expected at least one type application"
    Cons last rest -> pure $ foldr Arr last (reverse rest)
annExpr' :: Parser String Ann -> Parser String Ann
annExpr' p = parens annotation <|> annType
annType :: Parser String Ann
annType = T <$> ident

var :: Parser String Expr
var = do
  v <- ident
  t <- option U (string " :" *> skipSpaces *> annotation)
  pure $ Var t v

ident :: Parser String String
ident = alphaNum

app :: Parser String Expr -> Parser String Expr
app p = do
  _ <- pure 1 -- todo: remove?
  apps <- sepBy1 (aexpr p) (string " ")
  case apps of
    Cons e es -> pure $ foldl (App U) e es
    Nil -> fail $ "Expected at least one application"

parens :: forall a. Parser String a -> Parser String a
parens p = between (string "(") (string ")") p

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

alphas :: Array Char
alphas = [
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  ]

alphaNum :: Parser String String
alphaNum = do
  c <- oneOf alphas
  cs <- many (oneOf (alphas <> digits))
  pure $ fromCharArray $ cons c cs
