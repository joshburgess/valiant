module PgWire.Auth.ScramFieldsSpec (spec) where

import Data.ByteString qualified as BS
import Test.Hspec

-- Test the SCRAM field parser (internal, but we replicate the logic here)
spec :: Spec
spec = do
  describe "SCRAM field parsing" $ do
    it "parses r=nonce,s=salt,i=4096" $ do
      let input = "r=clientnonce+servernonce,s=c2FsdA==,i=4096"
          fields = parseScramFields input
      lookup "r" fields `shouldBe` Just "clientnonce+servernonce"
      lookup "s" fields `shouldBe` Just "c2FsdA=="
      lookup "i" fields `shouldBe` Just "4096"

    it "parses v=signature" $ do
      let input = "v=dGVzdHNpZw=="
          fields = parseScramFields input
      lookup "v" fields `shouldBe` Just "dGVzdHNpZw=="

    it "handles empty input" $ do
      let fields = parseScramFields ""
      fields `shouldBe` []

    it "handles single field" $ do
      let fields = parseScramFields "r=nonce"
      lookup "r" fields `shouldBe` Just "nonce"

    it "handles values containing equals signs" $ do
      -- base64 can contain '=' padding
      let fields = parseScramFields "s=abc=,i=100"
      -- The key is "s", value starts after first '='
      lookup "s" fields `shouldBe` Just "abc="

-- Replicated from ScramSHA256 module for testing
parseScramFields :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
parseScramFields bs =
  [ (key, val)
  | field <- BS.split 44 bs -- ','
  , let (key, rest) = BS.break (== 61) field -- '='
  , not (BS.null rest)
  , let val = BS.drop 1 rest
  ]
