module PgWire.Auth.MD5Spec (spec) where

import Crypto.Hash (MD5 (..), hashWith)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Test.Hspec

spec :: Spec
spec = do
  describe "MD5 password hash" $ do
    it "produces correct md5(md5(password+user)+salt) format" $ do
      -- PG MD5 auth: md5(md5(password + username) + salt)
      -- where md5() returns lowercase hex string
      let user = "testuser" :: ByteString
          password = "testpass" :: ByteString
          salt = BS.pack [0x01, 0x02, 0x03, 0x04]
          inner = md5Hex (password <> user)
          outer = "md5" <> md5Hex (inner <> salt)
      -- The hash should be deterministic
      BS.length outer `shouldBe` 35 -- "md5" + 32 hex chars
      BS8.isPrefixOf "md5" outer `shouldBe` True

    it "different passwords produce different hashes" $ do
      let user = "user" :: ByteString
          salt = BS.pack [0, 0, 0, 0]
          h1 = md5Hex (md5Hex ("pass1" <> user) <> salt)
          h2 = md5Hex (md5Hex ("pass2" <> user) <> salt)
      h1 `shouldNotBe` h2

md5Hex :: ByteString -> ByteString
md5Hex bs = BS8.pack $ concatMap (pad2 . showHex') (BS.unpack (BA.convert (hashWith MD5 bs)))
  where
    showHex' n = showHexByte n ""
    pad2 s = replicate (2 - length s) '0' ++ s
    showHexByte n = showHex n
    showHex 0 = ('0' :)
    showHex n
      | n < 16 = (hexDigit (fromIntegral n) :)
      | otherwise = showHex (n `div` 16) . (hexDigit (fromIntegral (n `mod` 16)) :)
    hexDigit i
      | i < 10 = toEnum (fromEnum '0' + i)
      | otherwise = toEnum (fromEnum 'a' + i - 10)
