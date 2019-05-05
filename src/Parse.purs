module Parse (parseExpr, parseAssign, Config(..), parseProgram) where

-- This module parses the lambda calculus, producing an AST
-- It's parameterised over the type of type annotations - construct a Config to tell
-- the parser how to interpret type annotations and it'll do the rest.

import Prelude

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy, sepBy1, option, optional)
import Text.Parsing.Parser.String (string, oneOf, skipSpaces, char, noneOf)

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
import Data.Map (Map)
import Data.Map as Map

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

parseProgram :: forall t. Config t -> String -> Either ParseError (Tuple (Map String (ExprT t)) (Map String t))
parseProgram cfg input = do
  defs <- runParser input (program cfg)
  pure $
    foldl (\(Tuple es ts) d -> case d of
                TypeDef s t -> Tuple es (Map.insert s t ts)
                ExprDef s e -> Tuple (Map.insert s e es) ts
          ) (Tuple mempty mempty) defs

-- A program is a sequence of definitions (of types and exprs)
-- One definition should be 'main'
data Def t = TypeDef String t | ExprDef String (ExprT t)

instance showDef :: Show t => Show (Def t) where
  show (TypeDef s t) = "TypeDef " <> show s <> " " <> show t
  show (ExprDef s t) = "ExprDef " <> show s <> " " <> show t

program :: forall t. Config t -> P (List (Def t))
program cfg = ((many newline) <* skipComments) *> sepBy (def cfg) ((some newline) <* skipComments)

skipComments :: P Unit
skipComments = optional $ many $ do
  _ <- string "--"
  _ <- many (noneOf ['\n'])
  optional newline

def :: forall t. Config t -> P (Def t)
def cfg = do
  name <- ident
  skipSpaces
  typedef cfg name <|> assign_ cfg name

typedef :: forall t. Config t -> String -> P (Def t)
typedef cfg name = do
  _ <- char ':'
  skipSpaces
  t <- annotation cfg
  pure $ TypeDef name t

assign_ :: forall t. Config t -> String -> P (Def t)
assign_ cfg name = do
  _ <- char '='
  skipSpaces
  e <- expr cfg
  pure $ ExprDef name e

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

newline :: P String
newline = string "\n"
