module ConnectionFeaturesSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.IORef
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Vector qualified as V
import Valiant
import PgWire.Connection
import PgWire.Connection.Config (parseConnString)
import PgWire.Protocol.Backend (TxStatus (..))
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "connectionStatus" $ do
    it "returns True for a live connection" $ do
      withTestConnection $ \conn -> do
        status <- connectionStatus conn
        status `shouldBe` True

  describe "transactionStatus" $ do
    it "returns TxIdle outside a transaction" $ do
      withTestConnection $ \conn -> do
        status <- transactionStatus conn
        status `shouldBe` TxIdle

  describe "parameterStatus" $ do
    it "returns server_version" $ do
      withTestConnection $ \conn -> do
        version <- parameterStatus conn "server_version"
        version `shouldSatisfy` isJust

    it "returns server_encoding" $ do
      withTestConnection $ \conn -> do
        enc <- parameterStatus conn "server_encoding"
        enc `shouldSatisfy` isJust

    it "returns Nothing for unknown params" $ do
      withTestConnection $ \conn -> do
        val <- parameterStatus conn "nonexistent_param"
        val `shouldBe` Nothing

  describe "serverVersion" $ do
    it "returns a positive integer" $ do
      withTestConnection $ \conn -> do
        version <- serverVersion conn
        case version of
          Just v -> v `shouldSatisfy` (> 100000)  -- at least PG 10
          Nothing -> expectationFailure "Expected version"

  describe "backendPid" $ do
    it "returns a positive PID" $ do
      withTestConnection $ \conn -> do
        let pid = backendPid conn
        pid `shouldSatisfy` (> 0)

  describe "reset" $ do
    it "reconnects and returns a working connection" $ do
      url <- requireDatabaseUrl
      conn <- connectString url
      conn2 <- reset conn
      -- conn is now closed; conn2 is the new connection
      (rows, _) <- simpleQuery conn2 "SELECT 1"
      length rows `shouldBe` 1
      close conn2

  describe "checkNotification" $ do
    it "returns Nothing when no notifications pending" $ do
      withTestConnection $ \conn -> do
        result <- checkNotification conn
        result `shouldBe` Nothing

  describe "describePrepared" $ do
    it "describes a prepared statement" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        -- Prepare a statement manually
        _ <- simpleQuery conn "PREPARE test_stmt (int4) AS SELECT id, name FROM users WHERE id = $1"
        (params, cols) <- describePrepared conn "test_stmt"
        -- Should have 1 param (int4 = OID 23)
        length (pdOids params) `shouldBe` 1
        -- Should have 2 columns
        length (cdFields cols) `shouldBe` 2
        _ <- simpleQuery conn "DEALLOCATE test_stmt"
        pure ()

  describe "escapeLiteral" $ do
    it "produces valid SQL when used in a query" $ do
      withTestConnection $ \conn -> do
        let escaped = escapeLiteral conn "O'Brien"
        (rows, _) <- simpleQuery conn ("SELECT " <> escaped)
        case rows of
          [[Just val]] -> val `shouldBe` "O'Brien"
          _ -> expectationFailure $ "Unexpected: " <> show rows

  describe "escapeIdentifier" $ do
    it "produces valid SQL for identifiers" $ do
      withTestConnection $ \conn -> do
        let escaped = escapeIdentifier conn "my col"
        (rows, _) <- simpleQuery conn ("SELECT 1 AS " <> escaped)
        length rows `shouldBe` 1

  describe "ping" $ do
    it "returns True for a reachable server" $ do
      url <- requireDatabaseUrl
      case parseConnString url of
        Right cfg -> do
          result <- ping cfg
          result `shouldBe` True
        Left _ -> expectationFailure "Failed to parse URL"

  describe "encryptPassword" $ do
    it "produces md5-prefixed hash" $ do
      let hash = encryptPassword "testuser" "testpass"
      BS.isPrefixOf "md5" hash `shouldBe` True
      BS.length hash `shouldBe` 35  -- "md5" + 32 hex chars

    it "produces different hashes for different passwords" $ do
      let h1 = encryptPassword "user" "pass1"
          h2 = encryptPassword "user" "pass2"
      h1 `shouldNotBe` h2

  describe "setTraceHandler" $ do
    it "captures send and recv traffic" $ do
      withTestConnection $ \conn -> do
        sendRef <- newIORef (0 :: Int)
        recvRef <- newIORef (0 :: Int)
        setTraceHandler conn $ \isSend _bytes ->
          if isSend
            then modifyIORef' sendRef (+ 1)
            else modifyIORef' recvRef (+ 1)
        _ <- simpleQuery conn "SELECT 1"
        sends <- readIORef sendRef
        recvs <- readIORef recvRef
        sends `shouldSatisfy` (> 0)
        recvs `shouldSatisfy` (> 0)

  describe "executeWithFold" $ do
    it "folds rows in constant memory" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        let countStmt :: Statement () (Int32, Text, Maybe Text)
            countStmt = mkStatement
              "SELECT id, name, email FROM users"
              [] ["id", "name", "email"] "<test>"
        count <- executeWithFold conn countStmt () $
          RowFold (0 :: Int) (\acc _ -> acc + 1)
        count `shouldBe` 5

  describe "copyInBinary" $ do
    it "bulk inserts via binary COPY" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        result <- copyInBinary conn
          "COPY users (name, email) FROM STDIN WITH (FORMAT binary)"
          2
          $ \sendRow -> do
            sendRow (V.fromList [Just (pgEncode ("BinAlice" :: BS.ByteString)), Just (pgEncode ("alice@bin.com" :: BS.ByteString))])
            sendRow (V.fromList [Just (pgEncode ("BinBob" :: BS.ByteString)), Nothing])
        copyRows result `shouldBe` 2
        (rows, _) <- simpleQuery conn "SELECT count(*) FROM users"
        case rows of
          [[Just n]] -> n `shouldBe` "2"
          _ -> expectationFailure $ "Expected 2, got: " <> show rows
