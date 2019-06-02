module NameGen (Gen(..), NameGen, genName) where

import Prelude
import Data.Array as A
import Data.String
import Data.Enum (toEnumWithDefaults)
import Data.String.CodeUnits (fromCharArray)
import Control.Monad.State
import Data.Set (Set)
import Data.Set as Set

type NameGen = State Gen

data Gen = Gen Int (Set String)

genName :: NameGen String
genName = do
  Gen i used <- get
  name <- genName'
  if Set.member name used
    then genName
    else pure name

genName' :: NameGen String
genName' = do
  Gen i used <- get
  put $ Gen (i + 1) used
  pure (genStr i)

genStr :: Int -> String
genStr i = fromCharArray (gen i)

gen :: Int -> Array Char
gen i | i <= 25 = A.singleton $ toEnumWithDefaults bottom top (97 + i)
      | otherwise = A.cons 'z' (gen (i - 26))
