module Parse (parseExpr, parseAssign, Config(..)) where

-- This module parses the lambda calculus, producing an AST
-- It's parameterised over the type of type annotations - construct a Config to tell
-- the parser how to interpret type annotations and it'll do the rest.

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

import Expr

type P = Parser String

type Config t = { parseType :: String -> t
                , mkNat :: Int -> ExprT t
                , nullType :: t
                , arrowType :: t -> t -> t
                }

parseExpr :: forall t. Config t -> String -> Either ParseError (ExprT t)
parseExpr cfg input = runParser input (expr cfg)

parseAssign :: forall t. Config t -> String -> Either ParseError (Tuple String (ExprT t))
parseAssign cfg input = runParser input (assign cfg)

assign :: forall t. Config t -> P (Tuple String (ExprT t))
assign cfg = do
  name <- ident
  _ <- string " = "
  e <- expr cfg
  pure $ Tuple name e

expr :: forall t. Config t -> P (ExprT t)
expr cfg = fix $ \p -> expr' cfg p

expr' :: forall t. Config t -> P (ExprT t) -> P (ExprT t)
expr' cfg p = app cfg p <|> aexpr cfg p

aexpr :: forall t. Config t -> P (ExprT t) -> P (ExprT t)
aexpr cfg p = parens (expr cfg) <|> number cfg <|> func cfg p <|> var cfg

number :: forall t. Config t -> P (ExprT t)
number cfg = do
  n <- readInt 10 <<< fromCharArray <$> some (oneOf digits)
  case fromNumber n of
    Just i -> pure $ cfg.mkNat i
    Nothing -> fail "cannot parse number"

func :: forall t. Config t -> P (ExprT t) -> P (ExprT t)
func cfg p = do
  _ <- (string "\\" <|> string "λ")
  v <- ident
  skipSpaces
  t <- option cfg.nullType (string ":" *> skipSpaces *> annotation cfg)
  _ <- string "."
  skipSpaces
  e <- expr' cfg p
  pure $ Fn cfg.nullType v t e

-- Parsing type annotations
annotation :: forall t. Config t -> P t
annotation cfg = fix $ \p -> annExpr cfg p

annExpr :: forall t. Config t -> P t -> P t
annExpr cfg p = annArrow cfg p <|> annExpr' cfg p

annArrow :: forall t. Config t -> P t -> P t
annArrow cfg p = do
  apps <- sepBy1 (annExpr' cfg p) (string " -> ")
  case reverse apps of
    Nil -> fail $ "Expected at least one type application"
    Cons last rest -> pure $ foldr cfg.arrowType last (reverse rest)

annExpr' :: forall t. Config t -> P t -> P t
annExpr' cfg p = parens (annotation cfg) <|> annType cfg

annType :: forall t. Config t -> P t
annType cfg = cfg.parseType <$> ident

var :: forall t. Config t -> P (ExprT t)
var cfg = do
  v <- ident
  t <- option cfg.nullType (string " :" *> skipSpaces *> annotation cfg)
  pure $ Var t v

ident :: P String
ident = alphaNum

app :: forall t. Config t -> P (ExprT t) -> P (ExprT t)
app cfg p = do
  _ <- pure 1 -- todo: remove?
  apps <- sepBy1 (aexpr cfg p) (string " ")
  case apps of
    Cons e es -> pure $ foldl (App cfg.nullType) e es
    Nil -> fail $ "Expected at least one application"

parens :: forall a. P a -> P a
parens p = between (string "(") (string ")") p

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

alphas :: Array Char
alphas = [
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  ]

alphaNum :: P String
alphaNum = do
  c <- oneOf alphas
  cs <- many (oneOf (alphas <> digits))
  pure $ fromCharArray $ cons c cs
