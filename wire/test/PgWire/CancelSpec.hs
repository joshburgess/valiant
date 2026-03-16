module PgWire.CancelSpec (spec) where

import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Test.Hspec

spec :: Spec
spec = do
  describe "CancelRequest message format" $ do
    it "is exactly 16 bytes" $ do
      let msg = buildCancelRequest 12345 67890
      BS.length msg `shouldBe` 16

    it "starts with length 16" $ do
      let msg = buildCancelRequest 1 2
      -- First 4 bytes: length = 16 = 0x00000010
      BS.take 4 msg `shouldBe` BS.pack [0, 0, 0, 16]

    it "contains cancel code 80877102" $ do
      let msg = buildCancelRequest 1 2
      -- Bytes 4-7: cancel code = 80877102 = 0x04D2162E
      let code = fromIntegral (BS.index msg 4) * 256 * 256 * 256
               + fromIntegral (BS.index msg 5) * 256 * 256
               + fromIntegral (BS.index msg 6) * 256
               + fromIntegral (BS.index msg 7) :: Int
      code `shouldBe` 80877102

    it "contains PID and key" $ do
      let msg = buildCancelRequest 42 99
      -- Bytes 8-11: PID
      let pid = fromIntegral (BS.index msg 8) * 256 * 256 * 256
              + fromIntegral (BS.index msg 9) * 256 * 256
              + fromIntegral (BS.index msg 10) * 256
              + fromIntegral (BS.index msg 11) :: Int
      pid `shouldBe` 42
      -- Bytes 12-15: key
      let key = fromIntegral (BS.index msg 12) * 256 * 256 * 256
              + fromIntegral (BS.index msg 13) * 256 * 256
              + fromIntegral (BS.index msg 14) * 256
              + fromIntegral (BS.index msg 15) :: Int
      key `shouldBe` 99

-- Replicate the cancel message builder for testing
buildCancelRequest :: Int32 -> Int32 -> BS.ByteString
buildCancelRequest pid key =
  LBS.toStrict . B.toLazyByteString $
    B.int32BE 16
      <> B.int32BE 80877102
      <> B.int32BE pid
      <> B.int32BE key
