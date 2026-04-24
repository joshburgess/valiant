module Main where

import Valiant.Plugin.CacheSpec qualified as CacheSpec
import Valiant.Plugin.ConfigSpec qualified as ConfigSpec
import Valiant.Plugin.ErrorCaseSpec qualified as ErrorCaseSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Valiant.Plugin.Config" ConfigSpec.spec
  describe "Valiant.Plugin.Cache" CacheSpec.spec
  describe "Valiant.Plugin.ErrorCases" ErrorCaseSpec.spec
