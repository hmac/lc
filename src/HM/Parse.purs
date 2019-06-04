module HM.Parse where

import Prelude

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy, sepBy1)
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

import HM

-- For simplicity, we require that all type variables are Titlecase and
-- all term variables are lowercase.

type P = Parser String

parseExpr :: String -> Either ParseError Expr
parseExpr input = runParser input (expr)

parseProgram :: String -> Either ParseError (Map String Expr)
parseProgram input = do
  defs <- runParser input program
  pure $ foldl (\m (Def s e) -> Map.insert s e m) mempty defs

-- A program is a sequence of definitions (of types and exprs)
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
expr' p = app p <|> aexpr p

aexpr :: P Expr -> P Expr
aexpr p = parens expr <|> unit_ <|> lam p <|> let_ p <|> bool <|> var

unit_ :: P Expr
unit_ = do
  _ <- string "unit"
  pure Unit

bool :: P Expr
bool = string "True" $> True
   <|> string "False" $> False

lam :: P Expr -> P Expr
lam p = do
  _ <- (string "\\" <|> string "λ")
  v <- termName
  skipSpaces
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Lam v e

let_ :: P Expr -> P Expr
let_ p = do
  _ <- string "let "
  x <- termName
  skipSpaces
  _ <- string "="
  skipSpaces
  e0 <- aexpr p
  skipSpaces
  _ <- string "in"
  skipSpaces
  e1 <- expr' p
  pure $ Let x e0 e1

var :: P Expr
var = do
  v <- termName
  pure $ Var v

app :: P Expr -> P Expr
app p = do
  apps <- sepBy1 (aexpr p) (string " ")
  case apps of
    Cons e es -> pure $ foldl App e es
    Nil -> fail $ "Expected at least one application"

termName :: P String
termName = do
  name <- lowercaseAlphaNum
  case name of
       "in" -> fail "'in' is a reserved keyword"
       n -> pure n

lowercaseAlphaNum :: P String
lowercaseAlphaNum = do
  c <- oneOf lowerAlphas
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
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  ]

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

parens :: forall a. P a -> P a
parens p = between (string "(") (string ")") p

newline :: P String
newline = string "\n"
