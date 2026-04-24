module Main where

import Valiant.CLI.CacheSpec qualified as CacheSpec
import Valiant.CLI.CustomTypesSpec qualified as CustomTypesSpec
import Valiant.CLI.DiscoverSpec qualified as DiscoverSpec
import Valiant.CLI.ErrorSpec qualified as ErrorSpec
import Valiant.CLI.HashSpec qualified as HashSpec
import Valiant.CLI.NamedParamsSpec qualified as NamedParamsSpec
import Valiant.CLI.SqlMetadataSpec qualified as SqlMetadataSpec
import Valiant.CLI.TypeMapSpec qualified as TypeMapSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Valiant.CLI.Hash" HashSpec.spec
  describe "Valiant.CLI.TypeMap" TypeMapSpec.spec
  describe "Valiant.CLI.Cache" CacheSpec.spec
  describe "Valiant.CLI.Discover" DiscoverSpec.spec
  describe "Valiant.CLI.Error" ErrorSpec.spec
  describe "Valiant.CLI.SqlMetadata" SqlMetadataSpec.spec
  describe "Valiant.CLI.NamedParams" NamedParamsSpec.spec
  describe "Valiant.CLI.CustomTypes" CustomTypesSpec.spec
