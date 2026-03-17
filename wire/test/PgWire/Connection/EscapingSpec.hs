module PgWire.Connection.EscapingSpec (spec) where

import PgWire.Connection (escapeLiteral, escapeIdentifier)
import Test.Hspec

spec :: Spec
spec = do
  describe "escapeLiteral" $ do
    it "wraps in single quotes" $
      escapeLiteral undefined "hello" `shouldBe` "'hello'"

    it "escapes single quotes" $
      escapeLiteral undefined "O'Brien" `shouldBe` "'O''Brien'"

    it "escapes backslashes" $
      escapeLiteral undefined "back\\slash" `shouldBe` "'back\\\\slash'"

    it "handles empty string" $
      escapeLiteral undefined "" `shouldBe` "''"

    it "handles multiple quotes" $
      escapeLiteral undefined "it's a 'test'" `shouldBe` "'it''s a ''test'''"

  describe "escapeIdentifier" $ do
    it "wraps in double quotes" $
      escapeIdentifier undefined "column" `shouldBe` "\"column\""

    it "escapes double quotes" $
      escapeIdentifier undefined "my\"col" `shouldBe` "\"my\"\"col\""

    it "handles spaces" $
      escapeIdentifier undefined "user table" `shouldBe` "\"user table\""

    it "handles empty string" $
      escapeIdentifier undefined "" `shouldBe` "\"\""

    it "handles reserved words" $
      escapeIdentifier undefined "select" `shouldBe` "\"select\""
