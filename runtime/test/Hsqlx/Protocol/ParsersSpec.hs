module Hsqlx.Protocol.ParsersSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32)
import Data.Vector qualified as V
import Data.Word (Word32)
import Hsqlx.Protocol.Backend
import Hsqlx.Protocol.Parsers
import Test.Hspec

-- Helper: build a big-endian Int16
bInt16 :: Int16 -> BS.ByteString
bInt16 = LBS.toStrict . B.toLazyByteString . B.int16BE

-- Helper: build a big-endian Int32
bInt32 :: Int32 -> BS.ByteString
bInt32 = LBS.toStrict . B.toLazyByteString . B.int32BE

-- Helper: build a big-endian Word32
bWord32 :: Word32 -> BS.ByteString
bWord32 = LBS.toStrict . B.toLazyByteString . B.word32BE

spec :: Spec
spec = do
  describe "parseBackendMsg" $ do
    it "parses AuthOk" $ do
      let payload = BS.pack [0, 0, 0, 0] -- auth type 0 = OK
      parseBackendMsg 82 payload `shouldBe` Right (Authentication AuthOk)

    it "parses AuthCleartextPassword" $ do
      let payload = BS.pack [0, 0, 0, 3]
      parseBackendMsg 82 payload `shouldBe` Right (Authentication AuthCleartextPassword)

    it "parses AuthMD5Password with salt" $ do
      let payload = BS.pack [0, 0, 0, 5, 0xDE, 0xAD, 0xBE, 0xEF]
      parseBackendMsg 82 payload
        `shouldBe` Right (Authentication (AuthMD5Password (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])))

    it "parses ReadyForQuery Idle" $ do
      parseBackendMsg 90 (BS.singleton 73) `shouldBe` Right (ReadyForQuery TxIdle)

    it "parses ReadyForQuery InTransaction" $ do
      parseBackendMsg 90 (BS.singleton 84) `shouldBe` Right (ReadyForQuery TxInTransaction)

    it "parses ReadyForQuery Failed" $ do
      parseBackendMsg 90 (BS.singleton 69) `shouldBe` Right (ReadyForQuery TxFailed)

    it "parses ParseComplete" $ do
      parseBackendMsg 49 BS.empty `shouldBe` Right ParseComplete

    it "parses BindComplete" $ do
      parseBackendMsg 50 BS.empty `shouldBe` Right BindComplete

    it "parses EmptyQueryResponse" $ do
      parseBackendMsg 73 BS.empty `shouldBe` Right EmptyQueryResponse

    it "parses NoData" $ do
      parseBackendMsg 110 BS.empty `shouldBe` Right NoData

    it "parses CloseComplete" $ do
      parseBackendMsg 51 BS.empty `shouldBe` Right CloseComplete

    it "returns error for unknown tag" $ do
      parseBackendMsg 255 BS.empty `shouldSatisfy` isLeft

  describe "parseCommandTag" $ do
    it "parses SELECT tag" $ do
      parseCommandTag "SELECT 42" `shouldBe` SelectTag 42

    it "parses INSERT tag" $ do
      parseCommandTag "INSERT 0 5" `shouldBe` InsertTag 5

    it "parses UPDATE tag" $ do
      parseCommandTag "UPDATE 3" `shouldBe` UpdateTag 3

    it "parses DELETE tag" $ do
      parseCommandTag "DELETE 1" `shouldBe` DeleteTag 1

    it "parses unknown tag" $ do
      parseCommandTag "CREATE TABLE" `shouldBe` OtherTag "CREATE TABLE"

  describe "ErrorResponse parsing" $ do
    it "parses structured error fields" $ do
      let payload =
            BS.concat
              [ BS.singleton 83 -- 'S'
              , "ERROR\0"
              , BS.singleton 67 -- 'C'
              , "42P01\0"
              , BS.singleton 77 -- 'M'
              , "relation \"foo\" does not exist\0"
              , BS.singleton 0 -- terminator
              ]
      case parseBackendMsg 69 payload of
        Right (ErrorResponse err) -> do
          pgSeverity err `shouldBe` "ERROR"
          pgCode err `shouldBe` "42P01"
          pgMessage err `shouldBe` "relation \"foo\" does not exist"
        other -> expectationFailure $ "Expected ErrorResponse, got: " <> show other

    it "parses error with detail and hint fields" $ do
      let payload =
            BS.concat
              [ BS.singleton 83, "ERROR\0"  -- severity
              , BS.singleton 67, "23505\0"  -- code
              , BS.singleton 77, "duplicate key\0"  -- message
              , BS.singleton 68, "Key (id)=(1) already exists.\0"  -- detail
              , BS.singleton 72, "Try a different id.\0"  -- hint
              , BS.singleton 0
              ]
      case parseBackendMsg 69 payload of
        Right (ErrorResponse err) -> do
          pgSeverity err `shouldBe` "ERROR"
          pgCode err `shouldBe` "23505"
          pgMessage err `shouldBe` "duplicate key"
          pgDetail err `shouldBe` Just "Key (id)=(1) already exists."
          pgHint err `shouldBe` Just "Try a different id."
        other -> expectationFailure $ "Expected ErrorResponse, got: " <> show other

  describe "DataRow parsing" $ do
    it "parses a row with zero columns" $ do
      let payload = bInt16 0
      parseBackendMsg 68 payload `shouldBe` Right (DataRow V.empty)

    it "parses a row with one non-NULL column" $ do
      let payload = BS.concat
            [ bInt16 1          -- 1 column
            , bInt32 5          -- length 5
            , "hello"           -- data
            ]
      parseBackendMsg 68 payload `shouldBe` Right (DataRow (V.singleton (Just "hello")))

    it "parses a row with one NULL column" $ do
      let payload = BS.concat
            [ bInt16 1          -- 1 column
            , bInt32 (-1)       -- NULL indicator
            ]
      parseBackendMsg 68 payload `shouldBe` Right (DataRow (V.singleton Nothing))

    it "parses a row with multiple columns including NULLs" $ do
      let payload = BS.concat
            [ bInt16 3                -- 3 columns
            , bInt32 3, "abc"         -- col 0: "abc"
            , bInt32 (-1)             -- col 1: NULL
            , bInt32 2, "xy"          -- col 2: "xy"
            ]
      parseBackendMsg 68 payload
        `shouldBe` Right (DataRow (V.fromList [Just "abc", Nothing, Just "xy"]))

    it "parses a row with an empty-string column (length 0)" $ do
      let payload = BS.concat
            [ bInt16 1
            , bInt32 0          -- length 0 (empty string, not NULL)
            ]
      parseBackendMsg 68 payload `shouldBe` Right (DataRow (V.singleton (Just "")))

  describe "RowDescription parsing" $ do
    it "parses a RowDescription with zero fields" $ do
      let payload = bInt16 0
      parseBackendMsg 84 payload `shouldBe` Right (RowDescription V.empty)

    it "parses a RowDescription with one field" $ do
      let payload = BS.concat
            [ bInt16 1            -- 1 field
            , "id\0"              -- field name
            , bWord32 16385       -- table OID
            , bInt16 1            -- column number
            , bWord32 23          -- type OID (int4)
            , bInt16 4            -- type size
            , bInt32 (-1)         -- type modifier
            , bInt16 0            -- format code (text)
            ]
      case parseBackendMsg 84 payload of
        Right (RowDescription fields) -> do
          V.length fields `shouldBe` 1
          let f = V.head fields
          fiName f `shouldBe` "id"
          fiTableOid f `shouldBe` 16385
          fiColumnNum f `shouldBe` 1
          fiTypeOid f `shouldBe` 23
          fiTypeSize f `shouldBe` 4
          fiTypeMod f `shouldBe` (-1)
          fiFormatCode f `shouldBe` 0
        other -> expectationFailure $ "Expected RowDescription, got: " <> show other

    it "parses a RowDescription with multiple fields" $ do
      let mkField name tOid colNum typeOid tSize = BS.concat
            [ name, "\0"
            , bWord32 tOid
            , bInt16 colNum
            , bWord32 typeOid
            , bInt16 tSize
            , bInt32 (-1)         -- type modifier
            , bInt16 1            -- format code (binary)
            ]
          payload = BS.concat
            [ bInt16 2
            , mkField "id" 16385 1 23 4
            , mkField "name" 16385 2 25 (-1)
            ]
      case parseBackendMsg 84 payload of
        Right (RowDescription fields) -> do
          V.length fields `shouldBe` 2
          fiName (fields V.! 0) `shouldBe` "id"
          fiTypeOid (fields V.! 0) `shouldBe` 23
          fiName (fields V.! 1) `shouldBe` "name"
          fiTypeOid (fields V.! 1) `shouldBe` 25
        other -> expectationFailure $ "Expected RowDescription, got: " <> show other

  describe "ParameterDescription parsing" $ do
    it "parses with zero parameters" $ do
      let payload = bInt16 0
      parseBackendMsg 116 payload `shouldBe` Right (ParameterDescription V.empty)

    it "parses with one parameter" $ do
      let payload = BS.concat [bInt16 1, bWord32 23]
      parseBackendMsg 116 payload `shouldBe` Right (ParameterDescription (V.singleton 23))

    it "parses with multiple parameters" $ do
      let payload = BS.concat [bInt16 3, bWord32 23, bWord32 25, bWord32 1184]
      parseBackendMsg 116 payload
        `shouldBe` Right (ParameterDescription (V.fromList [23, 25, 1184]))

  describe "ParameterStatus parsing" $ do
    it "parses server_version" $ do
      let payload = "server_version\0" <> "14.2\0"
      parseBackendMsg 83 payload `shouldBe` Right (ParameterStatus "server_version" "14.2")

    it "parses server_encoding" $ do
      let payload = "server_encoding\0" <> "UTF8\0"
      parseBackendMsg 83 payload `shouldBe` Right (ParameterStatus "server_encoding" "UTF8")

    it "parses client_encoding" $ do
      let payload = "client_encoding\0" <> "UTF8\0"
      parseBackendMsg 83 payload `shouldBe` Right (ParameterStatus "client_encoding" "UTF8")

  describe "AuthSASL parsing" $ do
    it "parses a single SASL mechanism" $ do
      let payload = BS.concat
            [ bInt32 10           -- auth type 10 = SASL
            , "SCRAM-SHA-256\0"
            , BS.singleton 0      -- terminator
            ]
      parseBackendMsg 82 payload
        `shouldBe` Right (Authentication (AuthSASL ["SCRAM-SHA-256"]))

    it "parses multiple SASL mechanisms" $ do
      let payload = BS.concat
            [ bInt32 10
            , "SCRAM-SHA-256\0"
            , "SCRAM-SHA-256-PLUS\0"
            , BS.singleton 0
            ]
      parseBackendMsg 82 payload
        `shouldBe` Right (Authentication (AuthSASL ["SCRAM-SHA-256", "SCRAM-SHA-256-PLUS"]))

    it "parses AuthSASLContinue" $ do
      let serverData = "r=nonce,s=salt,i=4096"
          payload = BS.concat [bInt32 11, serverData]
      parseBackendMsg 82 payload
        `shouldBe` Right (Authentication (AuthSASLContinue serverData))

    it "parses AuthSASLFinal" $ do
      let serverSig = "v=signature"
          payload = BS.concat [bInt32 12, serverSig]
      parseBackendMsg 82 payload
        `shouldBe` Right (Authentication (AuthSASLFinal serverSig))

  describe "BackendKeyData parsing" $ do
    it "parses process ID and secret key" $ do
      let payload = BS.concat [bInt32 12345, bInt32 67890]
      parseBackendMsg 75 payload `shouldBe` Right (BackendKeyData 12345 67890)

  describe "CommandComplete parsing" $ do
    it "parses SELECT 0 (empty result)" $ do
      parseBackendMsg 67 "SELECT 0\0" `shouldBe` Right (CommandComplete (SelectTag 0))

    it "parses INSERT with large count" $ do
      parseBackendMsg 67 "INSERT 0 1000\0" `shouldBe` Right (CommandComplete (InsertTag 1000))

  describe "NotificationResponse parsing" $ do
    it "parses a notification" $ do
      let payload = BS.concat
            [ bInt32 42           -- PID
            , "my_channel\0"
            , "some payload\0"
            ]
      parseBackendMsg 65 payload
        `shouldBe` Right (NotificationResponse 42 "my_channel" "some payload")

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
