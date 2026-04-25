module Main where

import Valiant.ErrorSpec qualified as ErrorSpec
import Valiant.Binary.ArraySpec qualified as ArraySpec
import Valiant.Binary.CompositeSpec qualified as CompositeSpec
import Valiant.Binary.DecodeSpec qualified as DecodeSpec
import Valiant.Binary.EncodeSpec qualified as EncodeSpec
import Valiant.Binary.HStoreSpec qualified as HStoreSpec
import Valiant.Binary.InetSpec qualified as InetSpec
import Valiant.Binary.MacAddrSpec qualified as MacAddrSpec
import Valiant.Binary.IntervalSpec qualified as IntervalSpec
import Valiant.Binary.JSONSpec qualified as JSONSpec
import Valiant.Binary.NewtypeSpec qualified as NewtypeSpec
import Valiant.Binary.PropertyHedgehogSpec qualified as PropertyHedgehogSpec
import Valiant.Binary.RangeSpec qualified as RangeSpec
import Valiant.Binary.RefineSpec qualified as RefineSpec
import Valiant.Binary.ScientificSpec qualified as ScientificSpec
import Valiant.Binary.UUIDSpec qualified as UUIDSpec
import Valiant.FromRowSpec qualified as FromRowSpec
import Valiant.NamedParamsSpec qualified as NamedParamsSpec
import Valiant.TupleSpec qualified as TupleSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Valiant.Binary.Encode" EncodeSpec.spec
  describe "Valiant.Binary.Decode" DecodeSpec.spec
  describe "Valiant.Binary.Array" ArraySpec.spec
  describe "Valiant.Binary.Composite" CompositeSpec.spec
  describe "Valiant.Binary.Range" RangeSpec.spec
  describe "Valiant.Binary.Refine" RefineSpec.spec
  describe "Valiant.Binary.Scientific" ScientificSpec.spec
  describe "Valiant.Binary.HStore" HStoreSpec.spec
  describe "Valiant.Binary.Inet" InetSpec.spec
  describe "Valiant.Binary.MacAddr" MacAddrSpec.spec
  describe "Valiant.Binary.Interval" IntervalSpec.spec
  describe "Valiant.Binary.UUID" UUIDSpec.spec
  describe "Valiant.Binary.JSON" JSONSpec.spec
  describe "Valiant.Binary.Newtype" NewtypeSpec.spec
  describe "Valiant.Binary.Property" PropertyHedgehogSpec.spec
  describe "Valiant.FromRow" FromRowSpec.spec
  describe "Valiant.Tuple" TupleSpec.spec
  describe "Valiant.NamedParams" NamedParamsSpec.spec
  describe "Valiant.Error" ErrorSpec.spec
