module Main where

import Hsqlx.Binary.ArraySpec qualified as ArraySpec
import Hsqlx.Binary.CompositeSpec qualified as CompositeSpec
import Hsqlx.Binary.DecodeSpec qualified as DecodeSpec
import Hsqlx.Binary.EncodeSpec qualified as EncodeSpec
import Hsqlx.Binary.IntervalSpec qualified as IntervalSpec
import Hsqlx.Binary.PropertySpec qualified as PropertySpec
import Hsqlx.Binary.RangeSpec qualified as RangeSpec
import Hsqlx.Binary.ScientificSpec qualified as ScientificSpec
import Hsqlx.FromRowSpec qualified as FromRowSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hsqlx.Binary.Encode" EncodeSpec.spec
  describe "Hsqlx.Binary.Decode" DecodeSpec.spec
  describe "Hsqlx.Binary.Array" ArraySpec.spec
  describe "Hsqlx.Binary.Composite" CompositeSpec.spec
  describe "Hsqlx.Binary.Range" RangeSpec.spec
  describe "Hsqlx.Binary.Scientific" ScientificSpec.spec
  describe "Hsqlx.Binary.Interval" IntervalSpec.spec
  describe "Hsqlx.Binary.Property" PropertySpec.spec
  describe "Hsqlx.FromRow" FromRowSpec.spec
