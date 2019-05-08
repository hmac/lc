module Test.Main where

import Prelude
import Effect (Effect)

import Test.Simple as Test.Simple
import Test.Simple.Parse as Test.Simple.Parse
import Test.SystemF.Parse as Test.SystemF.Parse
import Test.SystemF as Test.SystemF

main :: Effect Unit
main = do
  Test.Simple.main
  Test.Simple.Parse.main
  Test.SystemF.Parse.main
  Test.SystemF.main
