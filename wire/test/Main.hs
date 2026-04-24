module Main where

import PgWire.Auth.MD5Spec qualified as MD5Spec
import PgWire.Auth.ScramFieldsSpec qualified as ScramFieldsSpec
import PgWire.CancelSpec qualified as CancelSpec
import PgWire.Connection.ConfigSpec qualified as ConfigSpec
import PgWire.ErrorSpec qualified as ErrorSpec
import PgWire.MockServerSpec qualified as MockServerSpec
import PgWire.Pool.ConfigSpec qualified as PoolConfigSpec
import PgWire.Pool.NoThunksSpec qualified as NoThunksSpec
import PgWire.Pool.PropertySpec qualified as PoolPropertySpec
import PgWire.Pool.StateMachineSpec qualified as PoolStateMachineSpec
import PgWire.Protocol.BuildersSpec qualified as BuildersSpec
import PgWire.Protocol.OidSpec qualified as OidSpec
import PgWire.Connection.EscapingSpec qualified as EscapingSpec
import PgWire.Connection.FeaturesSpec qualified as FeaturesSpec
import PgWire.Protocol.FuzzSpec qualified as FuzzSpec
import PgWire.Protocol.ParsersSpec qualified as ParsersSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "PgWire.Protocol.Builders" BuildersSpec.spec
  describe "PgWire.Protocol.Parsers" ParsersSpec.spec
  describe "PgWire.Protocol.Fuzz" FuzzSpec.spec
  describe "PgWire.Protocol.Oid" OidSpec.spec
  describe "PgWire.Connection.Config" ConfigSpec.spec
  describe "PgWire.Pool.Config" PoolConfigSpec.spec
  describe "PgWire.Error" ErrorSpec.spec
  describe "PgWire.Auth.MD5" MD5Spec.spec
  describe "PgWire.Auth.ScramFields" ScramFieldsSpec.spec
  describe "PgWire.Cancel" CancelSpec.spec
  describe "PgWire.Connection.Features" FeaturesSpec.spec
  describe "PgWire.Connection.Escaping" EscapingSpec.spec
  describe "PgWire.MockServer" MockServerSpec.spec
  describe "PgWire.Pool.NoThunks" NoThunksSpec.spec
  describe "PgWire.Pool.Property" PoolPropertySpec.spec
  describe "PgWire.Pool.StateMachine" PoolStateMachineSpec.spec
