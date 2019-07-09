module Dependent.Parse where

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
