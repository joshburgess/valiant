module Main where

import Hsqlx.Auth.MD5Spec qualified as MD5Spec
import Hsqlx.Binary.ArraySpec qualified as ArraySpec
import Hsqlx.Binary.DecodeSpec qualified as DecodeSpec
import Hsqlx.Binary.EncodeSpec qualified as EncodeSpec
import Hsqlx.Binary.IntervalSpec qualified as IntervalSpec
import Hsqlx.Binary.ScientificSpec qualified as ScientificSpec
import Hsqlx.Connection.ConfigSpec qualified as ConfigSpec
import Hsqlx.FromRowSpec qualified as FromRowSpec
import Hsqlx.Protocol.BuildersSpec qualified as BuildersSpec
import Hsqlx.Protocol.ParsersSpec qualified as ParsersSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hsqlx.Protocol.Builders" BuildersSpec.spec
  describe "Hsqlx.Protocol.Parsers" ParsersSpec.spec
  describe "Hsqlx.Binary.Encode" EncodeSpec.spec
  describe "Hsqlx.Binary.Decode" DecodeSpec.spec
  describe "Hsqlx.Binary.Array" ArraySpec.spec
  describe "Hsqlx.Binary.Scientific" ScientificSpec.spec
  describe "Hsqlx.Binary.Interval" IntervalSpec.spec
  describe "Hsqlx.Connection.Config" ConfigSpec.spec
  describe "Hsqlx.Auth.MD5" MD5Spec.spec
  describe "Hsqlx.FromRow" FromRowSpec.spec
