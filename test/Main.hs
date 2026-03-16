module Main where

import Hsqlx.CLI.CacheSpec qualified as CacheSpec
import Hsqlx.CLI.CustomTypesSpec qualified as CustomTypesSpec
import Hsqlx.CLI.DiscoverSpec qualified as DiscoverSpec
import Hsqlx.CLI.ErrorSpec qualified as ErrorSpec
import Hsqlx.CLI.HashSpec qualified as HashSpec
import Hsqlx.CLI.SqlMetadataSpec qualified as SqlMetadataSpec
import Hsqlx.CLI.TypeMapSpec qualified as TypeMapSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hsqlx.CLI.Hash" HashSpec.spec
  describe "Hsqlx.CLI.TypeMap" TypeMapSpec.spec
  describe "Hsqlx.CLI.Cache" CacheSpec.spec
  describe "Hsqlx.CLI.Discover" DiscoverSpec.spec
  describe "Hsqlx.CLI.Error" ErrorSpec.spec
  describe "Hsqlx.CLI.SqlMetadata" SqlMetadataSpec.spec
  describe "Hsqlx.CLI.CustomTypes" CustomTypesSpec.spec
