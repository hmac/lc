module Test.Main where

import Prelude
import Effect (Effect)

import Test.Simple as Test.Simple
import Test.Simple.Parse as Test.Simple.Parse

main :: Effect Unit
main = do
  Test.Simple.main
  Test.Simple.Parse.main
