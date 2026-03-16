module Main where

import Hsqlx.Plugin.CacheSpec qualified as CacheSpec
import Hsqlx.Plugin.ConfigSpec qualified as ConfigSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hsqlx.Plugin.Config" ConfigSpec.spec
  describe "Hsqlx.Plugin.Cache" CacheSpec.spec
