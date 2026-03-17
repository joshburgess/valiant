module Hsqlx.Binary.InetSpec (spec) where

import Data.ByteString qualified as BS
import Hsqlx.Binary.Inet (PgInet (..), ipv4, ipv4Host, ipv6, ipv6Host, inetToText)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "inet IPv4 round-trip" $ do
    it "loopback /8" $ do
      let v = ipv4 127 0 0 1 8
      pgDecode (pgEncode v) `shouldBe` Right v

    it "private network /24" $ do
      let v = ipv4 192 168 1 0 24
      pgDecode (pgEncode v) `shouldBe` Right v

    it "host /32" $ do
      let v = ipv4Host 10 0 0 1
      pgDecode (pgEncode v) `shouldBe` Right v

    it "default route /0" $ do
      let v = ipv4 0 0 0 0 0
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "inet IPv6 round-trip" $ do
    it "loopback ::1/128" $ do
      let addr = BS.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
          v = ipv6Host addr
      pgDecode (pgEncode v) `shouldBe` Right v

    it "network /64" $ do
      let addr = BS.pack [0x20,0x01,0x0d,0xb8,0,0,0,0,0,0,0,0,0,0,0,0]
          v = ipv6 addr 64
      pgDecode (pgEncode v) `shouldBe` Right v

    it "all zeros ::/0" $ do
      let addr = BS.replicate 16 0
          v = ipv6 addr 0
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "inet encode size" $ do
    it "IPv4 produces 8 bytes (4 header + 4 address)" $ do
      BS.length (pgEncode (ipv4 192 168 1 0 24)) `shouldBe` 8

    it "IPv6 produces 20 bytes (4 header + 16 address)" $ do
      let addr = BS.replicate 16 0
      BS.length (pgEncode (ipv6 addr 64)) `shouldBe` 20

  describe "inet decode errors" $ do
    it "fails on too-short input" $ do
      let result = pgDecode (BS.pack [2, 24, 0]) :: Either String PgInet
      result `shouldSatisfy` isLeft

    it "fails on empty input" $ do
      let result = pgDecode BS.empty :: Either String PgInet
      result `shouldSatisfy` isLeft

    it "fails on address family mismatch" $ do
      -- Claim IPv6 family (3) but only 4 address bytes
      let result = pgDecode (BS.pack [3, 32, 0, 4, 10, 0, 0, 1]) :: Either String PgInet
      result `shouldSatisfy` isLeft

  describe "inet cidr flag" $ do
    it "preserves cidr flag on round-trip" $ do
      let v = (ipv4 192 168 1 0 24) { inetIsCidr = True }
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "inetToText" $ do
    it "renders IPv4 in CIDR notation" $
      inetToText (ipv4 192 168 1 0 24) `shouldBe` "192.168.1.0/24"

    it "renders IPv4 host" $
      inetToText (ipv4Host 10 0 0 1) `shouldBe` "10.0.0.1/32"

    it "renders IPv6 loopback" $ do
      let addr = BS.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
      inetToText (ipv6Host addr) `shouldBe` "::1/128"

    it "renders IPv6 all zeros" $ do
      let addr = BS.replicate 16 0
      inetToText (ipv6 addr 0) `shouldBe` "::/0"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
