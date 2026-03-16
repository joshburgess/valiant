module PgWire.Protocol.ParsersSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32)
import Data.Vector qualified as V
import Data.Word (Word32)
import PgWire.Protocol.Backend
import PgWire.Protocol.Parsers (parseBackendMsg, parseCommandTag)
import Test.Hspec

spec :: Spec
spec = do
  -- ── Authentication ──────────────────────────────────────────────
  describe "Authentication" $ do
    it "parses AuthOk" $
      parseBackendMsg 82 (int32 0) `shouldBe` Right (Authentication AuthOk)

    it "parses AuthCleartextPassword" $
      parseBackendMsg 82 (int32 3) `shouldBe` Right (Authentication AuthCleartextPassword)

    it "parses AuthMD5Password with 4-byte salt" $ do
      let salt = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
      parseBackendMsg 82 (int32 5 <> salt)
        `shouldBe` Right (Authentication (AuthMD5Password salt))

    it "parses AuthSASL with mechanism list" $ do
      let payload = int32 10 <> "SCRAM-SHA-256\0\0"
      case parseBackendMsg 82 payload of
        Right (Authentication (AuthSASL mechs)) ->
          mechs `shouldBe` ["SCRAM-SHA-256"]
        other -> expectationFailure $ "Expected AuthSASL, got: " <> show other

    it "parses AuthSASLContinue" $ do
      let serverData = "r=nonce,s=salt,i=4096"
      parseBackendMsg 82 (int32 11 <> serverData)
        `shouldBe` Right (Authentication (AuthSASLContinue serverData))

    it "parses AuthSASLFinal" $ do
      let serverData = "v=signature"
      parseBackendMsg 82 (int32 12 <> serverData)
        `shouldBe` Right (Authentication (AuthSASLFinal serverData))

    it "rejects unknown auth type" $
      parseBackendMsg 82 (int32 999) `shouldSatisfy` isLeft

  -- ── ReadyForQuery ───────────────────────────────────────────────
  describe "ReadyForQuery" $ do
    it "idle" $ parseBackendMsg 90 (BS.singleton 73) `shouldBe` Right (ReadyForQuery TxIdle)
    it "in transaction" $ parseBackendMsg 90 (BS.singleton 84) `shouldBe` Right (ReadyForQuery TxInTransaction)
    it "failed" $ parseBackendMsg 90 (BS.singleton 69) `shouldBe` Right (ReadyForQuery TxFailed)
    it "rejects unknown status byte" $ parseBackendMsg 90 (BS.singleton 0) `shouldSatisfy` isLeft
    it "rejects empty payload" $ parseBackendMsg 90 BS.empty `shouldSatisfy` isLeft

  -- ── Simple response messages ────────────────────────────────────
  describe "Simple responses" $ do
    it "ParseComplete" $ parseBackendMsg 49 BS.empty `shouldBe` Right ParseComplete
    it "BindComplete" $ parseBackendMsg 50 BS.empty `shouldBe` Right BindComplete
    it "CloseComplete" $ parseBackendMsg 51 BS.empty `shouldBe` Right CloseComplete
    it "NoData" $ parseBackendMsg 110 BS.empty `shouldBe` Right NoData
    it "EmptyQueryResponse" $ parseBackendMsg 73 BS.empty `shouldBe` Right EmptyQueryResponse
    it "CopyDoneMsg" $ parseBackendMsg 99 BS.empty `shouldBe` Right CopyDoneMsg

  -- ── CopyData ────────────────────────────────────────────────────
  describe "CopyDataMsg" $ do
    it "preserves payload" $
      parseBackendMsg 100 "hello world" `shouldBe` Right (CopyDataMsg "hello world")
    it "handles empty payload" $
      parseBackendMsg 100 BS.empty `shouldBe` Right (CopyDataMsg BS.empty)

  -- ── DataRow (hot path) ──────────────────────────────────────────
  describe "DataRow" $ do
    it "parses zero columns" $ do
      let payload = int16 0
      parseBackendMsg 68 payload `shouldBe` Right (DataRow V.empty)

    it "parses single non-null column" $ do
      let payload = int16 1 <> int32 5 <> "hello"
      case parseBackendMsg 68 payload of
        Right (DataRow v) -> do
          V.length v `shouldBe` 1
          v V.! 0 `shouldBe` Just "hello"
        other -> expectationFailure $ show other

    it "parses single NULL column" $ do
      let payload = int16 1 <> int32 (-1)
      case parseBackendMsg 68 payload of
        Right (DataRow v) -> do
          V.length v `shouldBe` 1
          v V.! 0 `shouldBe` Nothing
        other -> expectationFailure $ show other

    it "parses multiple columns with mixed nulls" $ do
      let payload = int16 3
            <> int32 2 <> "ab"    -- col 0: "ab"
            <> int32 (-1)          -- col 1: NULL
            <> int32 4 <> int32 42 -- col 2: 4 bytes (an int32)
      case parseBackendMsg 68 payload of
        Right (DataRow v) -> do
          V.length v `shouldBe` 3
          v V.! 0 `shouldBe` Just "ab"
          v V.! 1 `shouldBe` Nothing
          v V.! 2 `shouldBe` Just (int32 42)
        other -> expectationFailure $ show other

    it "parses empty string column (length 0)" $ do
      let payload = int16 1 <> int32 0
      case parseBackendMsg 68 payload of
        Right (DataRow v) -> do
          V.length v `shouldBe` 1
          v V.! 0 `shouldBe` Just BS.empty
        other -> expectationFailure $ show other

    it "parses many columns" $ do
      let n = 20
          colPayload = int32 1 <> "x"
          payload = int16 n <> BS.concat (replicate (fromIntegral n) colPayload)
      case parseBackendMsg 68 payload of
        Right (DataRow v) -> V.length v `shouldBe` fromIntegral n
        other -> expectationFailure $ show other

    it "fails on truncated column data" $
      parseBackendMsg 68 (int16 1 <> int32 100 <> "short") `shouldSatisfy` isLeft

    it "fails on truncated header" $
      parseBackendMsg 68 (BS.singleton 0) `shouldSatisfy` isLeft

  -- ── RowDescription ──────────────────────────────────────────────
  describe "RowDescription" $ do
    it "parses zero fields" $ do
      let payload = int16 0
      parseBackendMsg 84 payload `shouldBe` Right (RowDescription V.empty)

    it "parses single field" $ do
      let payload = int16 1
            <> "id\0"                -- name (NUL-terminated)
            <> word32 16384          -- table OID
            <> int16 1              -- column number
            <> word32 23            -- type OID (int4)
            <> int16 4              -- type size
            <> int32 (-1)           -- type modifier
            <> int16 0              -- format code (text)
      case parseBackendMsg 84 payload of
        Right (RowDescription v) -> do
          V.length v `shouldBe` 1
          let fi = v V.! 0
          fiName fi `shouldBe` "id"
          fiTableOid fi `shouldBe` 16384
          fiColumnNum fi `shouldBe` 1
          fiTypeOid fi `shouldBe` 23
          fiTypeSize fi `shouldBe` 4
          fiFormatCode fi `shouldBe` 0
        other -> expectationFailure $ show other

    it "parses multiple fields" $ do
      let mkField name oid =
              name <> "\0"
            <> word32 0 <> int16 0 <> word32 oid
            <> int16 (-1) <> int32 (-1) <> int16 1
          payload = int16 3
            <> mkField "id" 23
            <> mkField "name" 25
            <> mkField "email" 25
      case parseBackendMsg 84 payload of
        Right (RowDescription v) -> do
          V.length v `shouldBe` 3
          fiName (v V.! 0) `shouldBe` "id"
          fiName (v V.! 1) `shouldBe` "name"
          fiName (v V.! 2) `shouldBe` "email"
          fiTypeOid (v V.! 0) `shouldBe` 23
          fiTypeOid (v V.! 1) `shouldBe` 25
        other -> expectationFailure $ show other

  -- ── ParameterDescription ────────────────────────────────────────
  describe "ParameterDescription" $ do
    it "parses zero params" $
      parseBackendMsg 116 (int16 0) `shouldBe` Right (ParameterDescription V.empty)

    it "parses multiple param OIDs" $ do
      let payload = int16 2 <> word32 23 <> word32 25
      case parseBackendMsg 116 payload of
        Right (ParameterDescription v) -> do
          V.length v `shouldBe` 2
          v V.! 0 `shouldBe` 23
          v V.! 1 `shouldBe` 25
        other -> expectationFailure $ show other

  -- ── ParameterStatus ─────────────────────────────────────────────
  describe "ParameterStatus" $ do
    it "parses name=value pair" $ do
      let payload = "server_version\0" <> "16.2\0"
      parseBackendMsg 83 payload
        `shouldBe` Right (ParameterStatus "server_version" "16.2")

    it "handles empty value" $ do
      let payload = "param\0\0"
      parseBackendMsg 83 payload
        `shouldBe` Right (ParameterStatus "param" "")

  -- ── BackendKeyData ──────────────────────────────────────────────
  describe "BackendKeyData" $ do
    it "parses PID and key" $ do
      let payload = int32 12345 <> int32 67890
      parseBackendMsg 75 payload `shouldBe` Right (BackendKeyData 12345 67890)

    it "handles zero values" $
      parseBackendMsg 75 (int32 0 <> int32 0) `shouldBe` Right (BackendKeyData 0 0)

  -- ── NotificationResponse ────────────────────────────────────────
  describe "NotificationResponse" $ do
    it "parses PID, channel, payload" $ do
      let payload = int32 42 <> "my_channel\0" <> "hello\0"
      parseBackendMsg 65 payload
        `shouldBe` Right (NotificationResponse 42 "my_channel" "hello")

    it "handles empty payload string" $ do
      let payload = int32 1 <> "ch\0" <> "\0"
      parseBackendMsg 65 payload
        `shouldBe` Right (NotificationResponse 1 "ch" "")

  -- ── CopyInResponse / CopyOutResponse ────────────────────────────
  describe "CopyInResponse" $ do
    it "parses format and column formats" $ do
      let payload = BS.singleton 0 -- overall format: text
            <> int16 2           -- 2 columns
            <> int16 0           -- col 0: text
            <> int16 1           -- col 1: binary
      case parseBackendMsg 71 payload of
        Right (CopyInResponse fmt cols) -> do
          fmt `shouldBe` 0
          V.toList cols `shouldBe` [0, 1]
        other -> expectationFailure $ show other

  describe "CopyOutResponse" $ do
    it "parses format and column formats" $ do
      let payload = BS.singleton 1 <> int16 1 <> int16 1
      case parseBackendMsg 72 payload of
        Right (CopyOutResponse fmt cols) -> do
          fmt `shouldBe` 1
          V.toList cols `shouldBe` [1]
        other -> expectationFailure $ show other

  -- ── CommandComplete ─────────────────────────────────────────────
  describe "CommandComplete" $ do
    it "parses SELECT tag" $ do
      let payload = "SELECT 42\0"
      parseBackendMsg 67 payload `shouldBe` Right (CommandComplete (SelectTag 42))

    it "parses INSERT tag (with OID)" $ do
      let payload = "INSERT 0 5\0"
      parseBackendMsg 67 payload `shouldBe` Right (CommandComplete (InsertTag 5))

    it "parses UPDATE tag" $ do
      let payload = "UPDATE 100\0"
      parseBackendMsg 67 payload `shouldBe` Right (CommandComplete (UpdateTag 100))

    it "parses DELETE tag" $ do
      let payload = "DELETE 0\0"
      parseBackendMsg 67 payload `shouldBe` Right (CommandComplete (DeleteTag 0))

    it "parses CREATE TABLE as OtherTag" $ do
      let payload = "CREATE TABLE\0"
      parseBackendMsg 67 payload `shouldBe` Right (CommandComplete (OtherTag "CREATE TABLE"))

  describe "parseCommandTag" $ do
    it "SELECT 0" $ parseCommandTag "SELECT 0" `shouldBe` SelectTag 0
    it "SELECT 999999" $ parseCommandTag "SELECT 999999" `shouldBe` SelectTag 999999
    it "INSERT 0 1" $ parseCommandTag "INSERT 0 1" `shouldBe` InsertTag 1
    it "UPDATE 1" $ parseCommandTag "UPDATE 1" `shouldBe` UpdateTag 1
    it "DELETE 50" $ parseCommandTag "DELETE 50" `shouldBe` DeleteTag 50
    it "COPY 100" $ parseCommandTag "COPY 100" `shouldBe` OtherTag "COPY 100"
    it "BEGIN" $ parseCommandTag "BEGIN" `shouldBe` OtherTag "BEGIN"

  -- ── ErrorResponse / NoticeResponse ──────────────────────────────
  describe "ErrorResponse" $ do
    it "parses all standard fields" $ do
      let payload = BS.concat
            [ BS.singleton 83, "ERROR\0"              -- S: severity
            , BS.singleton 67, "42P01\0"              -- C: code
            , BS.singleton 77, "relation not found\0" -- M: message
            , BS.singleton 68, "some detail\0"        -- D: detail
            , BS.singleton 72, "try this\0"           -- H: hint
            , BS.singleton 80, "15\0"                 -- P: position
            , BS.singleton 87, "at function foo\0"    -- W: where
            , BS.singleton 115, "public\0"            -- s: schema
            , BS.singleton 116, "users\0"             -- t: table
            , BS.singleton 99, "email\0"              -- c: column
            , BS.singleton 100, "text\0"              -- d: data type
            , BS.singleton 110, "users_pkey\0"        -- n: constraint
            , BS.singleton 70, "parse.c\0"            -- F: file
            , BS.singleton 76, "42\0"                 -- L: line
            , BS.singleton 82, "exec_query\0"         -- R: routine
            , BS.singleton 0                          -- terminator
            ]
      case parseBackendMsg 69 payload of
        Right (ErrorResponse err) -> do
          pgSeverity err `shouldBe` "ERROR"
          pgCode err `shouldBe` "42P01"
          pgMessage err `shouldBe` "relation not found"
          pgDetail err `shouldBe` Just "some detail"
          pgHint err `shouldBe` Just "try this"
          pgPosition err `shouldBe` Just 15
          pgWhere err `shouldBe` Just "at function foo"
          pgSchema err `shouldBe` Just "public"
          pgTable err `shouldBe` Just "users"
          pgColumn err `shouldBe` Just "email"
          pgDataType err `shouldBe` Just "text"
          pgConstraint err `shouldBe` Just "users_pkey"
          pgFile err `shouldBe` Just "parse.c"
          pgLine err `shouldBe` Just 42
          pgRoutine err `shouldBe` Just "exec_query"
        other -> expectationFailure $ show other

    it "handles minimal error (severity + code + message only)" $ do
      let payload = BS.concat
            [ BS.singleton 83, "ERROR\0"
            , BS.singleton 67, "00000\0"
            , BS.singleton 77, "ok\0"
            , BS.singleton 0
            ]
      case parseBackendMsg 69 payload of
        Right (ErrorResponse err) -> do
          pgDetail err `shouldBe` Nothing
          pgHint err `shouldBe` Nothing
          pgPosition err `shouldBe` Nothing
          pgSchema err `shouldBe` Nothing
        other -> expectationFailure $ show other

  describe "NoticeResponse" $ do
    it "parses like ErrorResponse wrapped in PgNotice" $ do
      let payload = BS.concat
            [ BS.singleton 83, "WARNING\0"
            , BS.singleton 67, "01000\0"
            , BS.singleton 77, "be careful\0"
            , BS.singleton 0
            ]
      case parseBackendMsg 78 payload of
        Right (NoticeResponse (PgNotice err)) -> do
          pgSeverity err `shouldBe` "WARNING"
          pgMessage err `shouldBe` "be careful"
        other -> expectationFailure $ show other

  -- ── Unknown tags ────────────────────────────────────────────────
  describe "Unknown tags" $ do
    it "rejects tag 0" $ parseBackendMsg 0 BS.empty `shouldSatisfy` isLeft
    it "rejects tag 255" $ parseBackendMsg 255 BS.empty `shouldSatisfy` isLeft
    it "rejects tag 42" $ parseBackendMsg 42 BS.empty `shouldSatisfy` isLeft

-- Helpers -------------------------------------------------------------------

int16 :: Int16 -> BS.ByteString
int16 n = LBS.toStrict . B.toLazyByteString $ B.int16BE n

int32 :: Int32 -> BS.ByteString
int32 n = LBS.toStrict . B.toLazyByteString $ B.int32BE n

word32 :: Word32 -> BS.ByteString
word32 n = LBS.toStrict . B.toLazyByteString $ B.word32BE n

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
