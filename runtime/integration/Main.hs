module Main where

import ChaosSpec qualified
import ConnectionSpec qualified
import ConnectionFeaturesSpec qualified
import CopySpec qualified
import ExecuteSpec qualified
import LargeObjectSpec qualified
import NotifySpec qualified
import PipelineSpec qualified
import PoolSpec qualified
import SoundnessSpec qualified
import StreamingSpec qualified
import TransactionSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Connection" ConnectionSpec.spec
  describe "Connection.Features" ConnectionFeaturesSpec.spec
  describe "Execute" ExecuteSpec.spec
  describe "Pool" PoolSpec.spec
  describe "Transaction" TransactionSpec.spec
  describe "Copy" CopySpec.spec
  describe "Pipeline" PipelineSpec.spec
  describe "Streaming" StreamingSpec.spec
  describe "Notify" NotifySpec.spec
  describe "LargeObject" LargeObjectSpec.spec
  describe "Soundness" SoundnessSpec.spec
  describe "Chaos" ChaosSpec.spec
