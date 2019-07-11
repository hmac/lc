module Dependent.Parse where

import Prelude hiding (zero, between)

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy, sepBy1, optionMaybe, try)
import Text.Parsing.Parser.String (string, oneOf, skipSpaces, char, noneOf)

import Control.Lazy (fix)
import Data.Either (Either)
import Data.Array (cons, many)
import Control.Alt ((<|>))
import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map

import Dependent.Dependent (Expr(..))

-- For simplicity, we require that all type variables are Titlecase and
-- all term variables are lowercase.

type P = Parser String

parseExpr :: String -> Either ParseError Expr
parseExpr input = runParser input (expr)

parseProgram :: String -> Either ParseError (Map String Expr)
parseProgram input = do
  defs <- runParser input program
  pure $ foldl (\m (Def s e) -> Map.insert s e m) mempty defs

-- A program is a sequence of definitions
-- One definition should be 'main'
data Def = Def String Expr

program :: P (List Def)
program = (skipComments) *> sepBy def (skipComments)

skipComments :: P Unit
skipComments = many (comment <|> newline) *> pure unit

comment :: P String
comment = do
  _ <- string "--"
  _ <- many (noneOf ['\n'])
  newline

def :: P Def
def = do
  name <- termName
  skipSpaces
  _ <- char '='
  skipSpaces
  e <- expr
  pure $ Def name e

expr :: P Expr
expr = fix $ \p -> expr' p

expr' :: P Expr -> P Expr
expr' p = do
  e <- try (app p) <|> aexpr p
  skipSpaces
  t <- optionMaybe (annotation p)
  case t of
    Just t' -> pure (Ann e t')
    Nothing -> pure e

aexpr :: P Expr -> P Expr
aexpr p = unit_ <|> parens expr <|> lam p <|> pi p <|> nats p <|> var <|> ty

unit_ :: P Expr
unit_ = (string "()" *> pure Unit) <|> (string "Unit" *> pure UnitT)

nats :: P Expr -> P Expr
nats p = nat <|> zero <|> succ p <|> natElim p

nat :: P Expr
nat = string "Nat" *> pure Nat

zero :: P Expr
zero = string "Z" *> pure Zero

succ :: P Expr -> P Expr
succ p = do
  _ <- string "S"
  skipSpaces
  e <- expr' p
  pure $ Succ e

natElim :: P Expr -> P Expr
natElim p = do
  _ <- string "natElim"
  skipSpaces
  m <- expr' p
  skipSpaces
  mz <- expr' p
  skipSpaces
  ms <- expr' p
  skipSpaces
  k <- expr' p
  pure $ NatElim m mz ms k

annotation :: P Expr -> P Expr
annotation p = do
  _ <- string ":"
  skipSpaces
  e <- expr' p
  pure e

ty :: P Expr
ty = do
  _ <- string "Type"
  pure Type

lam :: P Expr -> P Expr
lam p = do
  _ <- (string "\\" <|> string "λ")
  v <- termName
  skipSpaces
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Lam v e

pi :: P Expr -> P Expr
pi p = do
  _ <- string "forall"
  skipSpaces
  _ <- string "("
  x <- termName
  skipSpaces
  _ <- string ":"
  skipSpaces
  t <- expr' p
  skipSpaces
  _ <- string ")"
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Pi x t e

var :: P Expr
var = do
  v <- termName
  pure (Var v)

app :: P Expr -> P Expr
app p = do
  apps <- sepBy1 (aexpr p) (string " ")
  case apps of
    Cons e es -> pure $ foldl App e es
    Nil -> fail $ "Expected at least one application"

termName :: P String
termName = lowercaseAlphaNum

typeName :: P String
typeName = uppercaseAlphaNum

lowercaseAlphaNum :: P String
lowercaseAlphaNum = do
  c <- oneOf lowerAlphas
  cs <- many (oneOf (alphas <> digits))
  pure $ fromCharArray $ cons c cs

uppercaseAlphaNum :: P String
uppercaseAlphaNum = do
  c <- oneOf upperAlphas
  cs <- many (oneOf (alphas <> digits))
  pure $ fromCharArray $ cons c cs

alphas :: Array Char
alphas = upperAlphas <> lowerAlphas

upperAlphas :: Array Char
upperAlphas = [
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  ]

lowerAlphas :: Array Char
lowerAlphas = [
  '_', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  ]

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

parens :: forall a. P a -> P a
parens p = between (string "(") (string ")") p

newline :: P String
newline = string "\n"
