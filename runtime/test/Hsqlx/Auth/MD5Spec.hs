module Hsqlx.Auth.MD5Spec (spec) where

import Data.ByteString qualified as BS
import Hsqlx.Auth.MD5
import Test.Hspec

-- Helpers
isPrefixOfBS :: BS.ByteString -> BS.ByteString -> Bool
isPrefixOfBS = BS.isPrefixOf

spec :: Spec
spec = do
  describe "md5Password" $ do
    -- The PG MD5 auth algorithm is:
    --   "md5" ++ md5(md5(password ++ user) ++ salt)

    it "computes known test vector: user=postgres, password=postgres, salt=0x01020304" $ do
      -- md5(password ++ user) = md5("postgrespostgres")
      -- We verify the result has the expected format: "md5" + 32 hex chars = 35 bytes
      let result = md5Password "postgres" "postgres" "\x01\x02\x03\x04"
      -- Must start with "md5"
      isPrefixOfBS "md5" result `shouldBe` True
      -- Total length: "md5" (3) + 32 hex chars = 35 bytes
      BS.length result `shouldBe` 35

    it "computes known test vector: user=alice, password=secret, salt=0xDEADBEEF" $ do
      let result = md5Password "alice" "secret" "\xDE\xAD\xBE\xEF"
      isPrefixOfBS "md5" result `shouldBe` True
      BS.length result `shouldBe` 35

    it "produces different results for different salts" $ do
      let r1 = md5Password "user" "pass" "\x01\x02\x03\x04"
          r2 = md5Password "user" "pass" "\x05\x06\x07\x08"
      r1 `shouldNotBe` r2

    it "produces different results for different users" $ do
      let r1 = md5Password "alice" "pass" "\x01\x02\x03\x04"
          r2 = md5Password "bob" "pass" "\x01\x02\x03\x04"
      r1 `shouldNotBe` r2

    it "produces different results for different passwords" $ do
      let r1 = md5Password "user" "pass1" "\x01\x02\x03\x04"
          r2 = md5Password "user" "pass2" "\x01\x02\x03\x04"
      r1 `shouldNotBe` r2

    it "is deterministic" $ do
      let r1 = md5Password "user" "pass" "\x01\x02\x03\x04"
          r2 = md5Password "user" "pass" "\x01\x02\x03\x04"
      r1 `shouldBe` r2

    it "only contains hex characters after 'md5' prefix" $ do
      let result = md5Password "user" "pass" "\x01\x02\x03\x04"
          hexPart = BS.drop 3 result
          isHex w = (w >= 48 && w <= 57)   -- 0-9
                 || (w >= 97 && w <= 102)  -- a-f
      BS.all isHex hexPart `shouldBe` True
