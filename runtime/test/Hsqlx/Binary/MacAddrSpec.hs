module Hsqlx.Binary.MacAddrSpec (spec) where

import Data.ByteString qualified as BS
import Hsqlx.Binary.MacAddr (PgMacAddr (..), macAddr, macAddrToText)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "macaddr round-trip" $ do
    it "standard 6-byte address" $ do
      let v = macAddr 0x08 0x00 0x2b 0x01 0x02 0x03
      pgDecode (pgEncode v) `shouldBe` Right v

    it "all zeros" $ do
      let v = macAddr 0 0 0 0 0 0
      pgDecode (pgEncode v) `shouldBe` Right v

    it "all ones" $ do
      let v = macAddr 0xff 0xff 0xff 0xff 0xff 0xff
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "macaddr8 round-trip" $ do
    it "8-byte address" $ do
      let v = PgMacAddr (BS.pack [0x08, 0x00, 0x2b, 0xfe, 0xff, 0x01, 0x02, 0x03])
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "macaddr encode size" $ do
    it "6-byte produces 6 bytes" $
      BS.length (pgEncode (macAddr 1 2 3 4 5 6)) `shouldBe` 6

    it "8-byte produces 8 bytes" $
      BS.length (pgEncode (PgMacAddr (BS.replicate 8 0))) `shouldBe` 8

  describe "macaddr decode errors" $ do
    it "fails on 5 bytes" $ do
      let result = pgDecode (BS.pack [1, 2, 3, 4, 5]) :: Either String PgMacAddr
      result `shouldSatisfy` isLeft

    it "fails on empty" $ do
      let result = pgDecode BS.empty :: Either String PgMacAddr
      result `shouldSatisfy` isLeft

  describe "macAddrToText" $ do
    it "renders with colons" $
      macAddrToText (macAddr 0x08 0x00 0x2b 0x01 0x02 0x03) `shouldBe` "08:00:2b:01:02:03"

    it "pads single hex digits" $
      macAddrToText (macAddr 0x01 0x02 0x03 0x04 0x05 0x06) `shouldBe` "01:02:03:04:05:06"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
