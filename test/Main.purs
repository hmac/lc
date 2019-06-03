module Test.Main where

import Prelude
import Effect (Effect)

import Test.Simple as Test.Simple
import Test.Simple.Parse as Test.Simple.Parse
import Test.SystemF.Parse as Test.SystemF.Parse
import Test.SystemF as Test.SystemF
import Test.HM as Test.HM

import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  Test.Simple.main
  Test.Simple.Parse.main
  Test.SystemF.Parse.main
  Test.SystemF.main
  Test.HM.main
