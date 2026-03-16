module Main where

import PgWire.Auth.MD5Spec qualified as MD5Spec
import PgWire.Auth.ScramFieldsSpec qualified as ScramFieldsSpec
import PgWire.Connection.ConfigSpec qualified as ConfigSpec
import PgWire.Protocol.BuildersSpec qualified as BuildersSpec
import PgWire.Protocol.OidSpec qualified as OidSpec
import PgWire.Protocol.ParsersSpec qualified as ParsersSpec
import PgWire.CancelSpec qualified as CancelSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "PgWire.Protocol.Builders" BuildersSpec.spec
  describe "PgWire.Protocol.Parsers" ParsersSpec.spec
  describe "PgWire.Protocol.Oid" OidSpec.spec
  describe "PgWire.Connection.Config" ConfigSpec.spec
  describe "PgWire.Auth.MD5" MD5Spec.spec
  describe "PgWire.Auth.ScramFields" ScramFieldsSpec.spec
  describe "PgWire.Cancel" CancelSpec.spec
