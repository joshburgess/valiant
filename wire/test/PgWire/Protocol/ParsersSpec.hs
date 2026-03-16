module PgWire.Protocol.ParsersSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import PgWire.Protocol.Backend
import PgWire.Protocol.Parsers (parseBackendMsg, parseCommandTag)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseBackendMsg" $ do
    it "parses ReadyForQuery (idle)" $ do
      parseBackendMsg 90 (BS.singleton 73) `shouldBe` Right (ReadyForQuery TxIdle)

    it "parses ReadyForQuery (in transaction)" $ do
      parseBackendMsg 90 (BS.singleton 84) `shouldBe` Right (ReadyForQuery TxInTransaction)

    it "parses ReadyForQuery (failed)" $ do
      parseBackendMsg 90 (BS.singleton 69) `shouldBe` Right (ReadyForQuery TxFailed)

    it "parses ParseComplete" $ do
      parseBackendMsg 49 BS.empty `shouldBe` Right ParseComplete

    it "parses BindComplete" $ do
      parseBackendMsg 50 BS.empty `shouldBe` Right BindComplete

    it "parses CloseComplete" $ do
      parseBackendMsg 51 BS.empty `shouldBe` Right CloseComplete

    it "parses NoData" $ do
      parseBackendMsg 110 BS.empty `shouldBe` Right NoData

    it "parses EmptyQueryResponse" $ do
      parseBackendMsg 73 BS.empty `shouldBe` Right EmptyQueryResponse

    it "parses AuthOk" $ do
      let payload = int32 0
      parseBackendMsg 82 payload `shouldBe` Right (Authentication AuthOk)

    it "parses AuthCleartextPassword" $ do
      let payload = int32 3
      parseBackendMsg 82 payload `shouldBe` Right (Authentication AuthCleartextPassword)

    it "parses AuthMD5Password with salt" $ do
      let salt = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
          payload = int32 5 <> salt
      parseBackendMsg 82 payload `shouldBe` Right (Authentication (AuthMD5Password salt))

    it "parses CopyDoneMsg" $ do
      parseBackendMsg 99 BS.empty `shouldBe` Right CopyDoneMsg

    it "parses CopyDataMsg" $ do
      let payload = "hello world"
      parseBackendMsg 100 payload `shouldBe` Right (CopyDataMsg payload)

    it "parses ErrorResponse with all fields" $ do
      let payload = BS.concat
            [ BS.singleton 83, "ERROR\0"     -- severity
            , BS.singleton 67, "42P01\0"     -- code
            , BS.singleton 77, "table not found\0" -- message
            , BS.singleton 0                 -- terminator
            ]
      case parseBackendMsg 69 payload of
        Right (ErrorResponse err) -> do
          pgSeverity err `shouldBe` "ERROR"
          pgCode err `shouldBe` "42P01"
          pgMessage err `shouldBe` "table not found"
        other -> expectationFailure $ "Expected ErrorResponse, got: " <> show other

    it "rejects unknown tags" $ do
      parseBackendMsg 255 BS.empty `shouldSatisfy` isLeft

  describe "parseCommandTag" $ do
    it "parses SELECT count" $ do
      parseCommandTag "SELECT 42" `shouldBe` SelectTag 42

    it "parses INSERT oid count" $ do
      parseCommandTag "INSERT 0 5" `shouldBe` InsertTag 5

    it "parses UPDATE count" $ do
      parseCommandTag "UPDATE 100" `shouldBe` UpdateTag 100

    it "parses DELETE count" $ do
      parseCommandTag "DELETE 0" `shouldBe` DeleteTag 0

    it "parses unknown as OtherTag" $ do
      parseCommandTag "CREATE TABLE" `shouldBe` OtherTag "CREATE TABLE"

-- Helpers
int32 :: Int -> BS.ByteString
int32 n = LBS.toStrict . B.toLazyByteString . B.int32BE $ fromIntegral n

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
