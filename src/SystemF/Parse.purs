module SystemF.Parse where


import Prelude

import Text.Parsing.Parser (Parser, ParseError, runParser, fail)
import Text.Parsing.Parser.Combinators (between, sepBy, sepBy1, option, optional, try)
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

import SystemF

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
aexpr p = try ty <|> parens expr <|> number <|> lam p <|> forall p <|> var

number :: P Expr
number = do
  n <- readInt 10 <<< fromCharArray <$> some (oneOf digits)
  case fromNumber n of
    Just i -> pure $ mkNat i
    Nothing -> fail "cannot parse number"

mkNat :: Int -> Expr
mkNat 0 = zero
mkNat i | i < 0 = mkNat 0
        | otherwise = succ (mkNat (i - 1))

zero :: Expr
zero
  = Forall U "X" $
    Lam U "f" (Arr (TVar "X") (TVar "X")) $
      Lam U "x" (TVar "X") $
        (Var U "x")

succ :: Expr -> Expr
succ (Forall _ "X" (Lam _ "f" tf (Lam _ "x" tx e)))
  = (Forall U "X" (Lam U "f" tf (Lam U "x" tx (App U (Var U "f") e))))
succ _ = zero -- should never be reached

lam :: P Expr -> P Expr
lam p = do
  _ <- (string "\\" <|> string "λ")
  v <- termName
  skipSpaces
  t <- option U (string ":" *> skipSpaces *> annotation)
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Lam U v t e

forall :: P Expr -> P Expr
forall p = do
  _ <- string "forall"
  skipSpaces
  t <- typeName
  _ <- string "."
  skipSpaces
  e <- expr' p
  pure $ Forall U t e

-- Parsing type annotations
annotation :: P Type
annotation = fix $ \p -> annExpr p

annExpr :: P Type -> P Type
annExpr p = annArrow p <|> annExpr' p

annArrow :: P Type -> P Type
annArrow p = do
  apps <- sepBy1 (annExpr' p) (string " -> ")
  case reverse apps of
    Nil -> fail $ "Expected at least one type application"
    Cons last rest -> pure $ foldr Arr last (reverse rest)

annExpr' :: P Type -> P Type
annExpr' p = parens (annotation) <|> annType

annType :: P Type
annType = do
  t <- typeName
  pure $ if t == "T" then T else TVar t

var :: P Expr
var = do
  v <- termName
  t <- option U (string " :" *> skipSpaces *> annotation)
  pure $ Var t v

app :: P Expr -> P Expr
app p = do
  apps <- sepBy1 (aexpr p) (string " ")
  case apps of
    Cons e es -> pure $ foldl (App U) e es
    Nil -> fail $ "Expected at least one application"

ty :: P Expr
ty = Ty <$> annotation

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
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  ]

digits :: Array Char
digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

parens :: forall a. P a -> P a
parens p = between (string "(") (string ")") p

newline :: P String
newline = string "\n"
