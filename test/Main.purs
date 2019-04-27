module Test.Main where

import Prelude
import Effect (Effect)

import Test.ParseSimple as TestParseSimple
import Test.Simple as TestSimple

main :: Effect Unit
main = do
  TestParseSimple.main
  TestSimple.main
