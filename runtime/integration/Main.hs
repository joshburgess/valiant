module Main where

import ConnectionSpec qualified
import ExecuteSpec qualified
import PoolSpec qualified
import TransactionSpec qualified
import CopySpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Connection" ConnectionSpec.spec
  describe "Execute" ExecuteSpec.spec
  describe "Pool" PoolSpec.spec
  describe "Transaction" TransactionSpec.spec
  describe "Copy" CopySpec.spec
