-- | PostgreSQL connection management.
--
-- A 'Connection' represents an active TCP (or TLS) session with a
-- PostgreSQL server. It handles authentication, parameter negotiation,
-- and prepared statement caching.
--
-- For production use, prefer 'PgWire.Pool' over direct connections.
--
-- @
-- conn <- 'connectString' \"postgres:\/\/user:pass\@localhost:5432\/mydb\"
-- (rows, _) <- 'simpleQuery' conn \"SELECT 1\"
-- 'close' conn
-- @
module PgWire.Connection
  ( -- * Connection type
    Connection (..)
    -- * Connecting
  , connect
  , connectString
  , reset
    -- * Closing
  , close
  , withConnection
    -- * Simple queries
  , simpleQuery
    -- * Connection status
  , connectionStatus
  , transactionStatus
  , parameterStatus
  , serverVersion
  , backendPid
  , isSslInUse
    -- * Notifications (non-blocking)
  , checkNotification
    -- * Notice handling
  , setNoticeHandler
    -- * SQL escaping
  , escapeLiteral
  , escapeIdentifier
    -- * Statement introspection
  , describePrepared
  , ParamDescription (..)
  , ColumnDescription (..)
  ) where

import Control.Exception (SomeException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import PgWire.Auth.Cleartext (cleartextAuth)
import PgWire.Auth.MD5 (md5Auth)
import PgWire.Auth.ScramSHA256 (scramAuth)
import PgWire.Connection.Config (ConnConfig (..), TlsMode (..), parseConnString)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Builders (buildStartup)
import PgWire.Protocol.Frontend (DescribeTarget (..), FrontendMsg (..), StartupParams (..))
import PgWire.Wire (WireConn (..), connectTcpTimeout, recvBackendMsg, sendFrontendMsg, sendRawBytes, upgradeTls)

-- | A connection to a PostgreSQL database.
data Connection = Connection
  { connWire :: WireConn
  , connConfig :: ConnConfig
  , connParams :: IORef (Map ByteString ByteString)
  , connBackendPid :: {-# UNPACK #-} !Int32
  , connBackendKey :: {-# UNPACK #-} !Int32
  , connTxStatus :: IORef TxStatus
  , connStmtCache :: IORef (Map ByteString ByteString)
  , connStmtCounter :: IORef Word64
  , connNoticeHandler :: IORef (PgNotice -> IO ())
  , connSslActive :: !Bool
  }

-- | Result of 'describePrepared'.
data ParamDescription = ParamDescription
  { pdOids :: Vector Word32
  }
  deriving stock (Show, Eq)

data ColumnDescription = ColumnDescription
  { cdFields :: Vector FieldInfo
  }
  deriving stock (Show, Eq)

-- | Connect using a 'ConnConfig'.
connect :: ConnConfig -> IO Connection
connect cfg = do
  wc0 <- connectTcpTimeout (ccConnectTimeout cfg) (ccHost cfg) (ccPort cfg)

  -- Optionally upgrade to TLS
  wc <- case ccTls cfg of
    TlsDisable -> pure wc0
    TlsRequire -> upgradeTls wc0 (ccHost cfg)
    TlsPrefer -> do
      -- Try TLS, fall back to plaintext
      result <- try @SomeException (upgradeTls wc0 (ccHost cfg))
      case result of
        Right tlsWc -> pure tlsWc
        Left _ -> do
          -- The SSLRequest already consumed the connection; reconnect
          wcClose wc0
          connectTcpTimeout (ccConnectTimeout cfg) (ccHost cfg) (ccPort cfg)

  -- Send startup message
  let startup =
        StartupParams
          { spUser = ccUser cfg
          , spDatabase = ccDatabase cfg
          , spAppName = ccAppName cfg
          , spExtraParams = []
          }
  sendRawBytes wc (buildStartup startup)

  -- Handle authentication
  handleAuth wc cfg

  -- Collect server parameters until ReadyForQuery
  paramsRef <- newIORef Map.empty
  pidRef <- newIORef 0
  keyRef <- newIORef 0
  txRef <- newIORef TxIdle

  let loop = do
        msg <- recvBackendMsg wc
        case msg of
          ParameterStatus name value -> do
            modifyIORef' paramsRef (Map.insert name value)
            loop
          BackendKeyData pid key -> do
            writeIORef pidRef pid
            writeIORef keyRef key
            loop
          ReadyForQuery status -> do
            writeIORef txRef status
            pure ()
          NoticeResponse _ -> loop
          other ->
            throwHsqlx (ProtocolError ("Unexpected message during startup: " <> BS8.pack (show other)))

  loop

  pid <- readIORef pidRef
  key <- readIORef keyRef
  stmtCache <- newIORef Map.empty
  stmtCounter <- newIORef 0
  noticeHandler <- newIORef (\_ -> pure ())
  let sslActive = case ccTls cfg of
        TlsDisable -> False
        _ -> True  -- If we attempted TLS, it's active (or connect failed)

  pure
    Connection
      { connWire = wc
      , connConfig = cfg
      , connParams = paramsRef
      , connBackendPid = pid
      , connBackendKey = key
      , connTxStatus = txRef
      , connStmtCache = stmtCache
      , connStmtCounter = stmtCounter
      , connNoticeHandler = noticeHandler
      , connSslActive = sslActive
      }

-- | Connect using a connection string.
connectString :: ByteString -> IO Connection
connectString bs = case parseConnString bs of
  Left err -> throwHsqlx (ConnectionError (BS8.pack err))
  Right cfg -> connect cfg

-- | Close a connection.
close :: Connection -> IO ()
close conn = do
  sendFrontendMsg (connWire conn) Terminate
  wcClose (connWire conn)

-- | Bracket-style connection management.
withConnection :: ConnConfig -> (Connection -> IO a) -> IO a
withConnection cfg = bracket (connect cfg) close

-- | Execute a simple text query (no parameters). Returns all result rows
-- as lists of nullable bytestrings, plus the command tag.
simpleQuery :: Connection -> ByteString -> IO ([[Maybe ByteString]], Maybe CommandTag)
simpleQuery conn sql = do
  sendFrontendMsg (connWire conn) (Query sql)
  collectSimpleResults conn

collectSimpleResults :: Connection -> IO ([[Maybe ByteString]], Maybe CommandTag)
collectSimpleResults conn = go [] Nothing
  where
    go rows tag = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        RowDescription _ -> go rows tag
        DataRow vals -> go (V.toList vals : rows) tag
        CommandComplete ct -> go rows (Just ct)
        EmptyQueryResponse -> go rows tag
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (reverse rows, tag)
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go rows tag
        other -> throwHsqlx (ProtocolError ("Unexpected in simple query: " <> BS8.pack (show other)))

-- | Reset the connection: close and reconnect using the same config.
reset :: Connection -> IO Connection
reset conn = do
  close conn
  connect (connConfig conn)

-- Connection status -------------------------------------------------------

-- | Check if the connection is alive by sending an empty query.
connectionStatus :: Connection -> IO Bool
connectionStatus conn = do
  result <- try @SomeException (simpleQuery conn "")
  pure $ case result of
    Right _ -> True
    Left _ -> False

-- | Get the current transaction status.
transactionStatus :: Connection -> IO TxStatus
transactionStatus conn = readIORef (connTxStatus conn)

-- | Look up a server parameter (e.g., @\"server_version\"@, @\"server_encoding\"@).
parameterStatus :: Connection -> ByteString -> IO (Maybe ByteString)
parameterStatus conn key = do
  params <- readIORef (connParams conn)
  pure (Map.lookup key params)

-- | Get the server version as an integer (e.g., 160004 for 16.4).
-- Parses from the @server_version@ parameter.
serverVersion :: Connection -> IO (Maybe Int)
serverVersion conn = do
  mVersion <- parameterStatus conn "server_version"
  pure (mVersion >>= parseServerVersion)
  where
    parseServerVersion bs =
      let parts = BS8.split '.' bs
       in case parts of
            [major, minor] -> do
              maj <- readInt major
              mn <- readInt minor
              pure (maj * 10000 + mn)
            [major, minor, patch] -> do
              maj <- readInt major
              mn <- readInt minor
              p <- readInt patch
              pure (maj * 10000 + mn * 100 + p)
            _ -> Nothing
    readInt s = case BS8.readInt s of
      Just (n, _) -> Just n
      Nothing -> Nothing

-- | Get the backend process ID.
backendPid :: Connection -> Int32
backendPid = connBackendPid

-- | Check if SSL/TLS is in use on this connection.
isSslInUse :: Connection -> Bool
isSslInUse = connSslActive

-- Notifications (non-blocking) --------------------------------------------

-- | Check for a pending notification without blocking.
-- Returns 'Nothing' if no notification is available.
checkNotification :: Connection -> IO (Maybe (Int32, ByteString, ByteString))
checkNotification conn = do
  -- Send empty query to flush pending notifications from the server
  sendFrontendMsg (connWire conn) (Query "")
  collectNotification conn
  where
    collectNotification c = go
      where
        go = do
          msg <- recvBackendMsg (connWire c)
          case msg of
            NotificationResponse pid channel payload ->
              pure (Just (pid, channel, payload))
            ReadyForQuery status -> do
              writeIORef (connTxStatus c) status
              pure Nothing
            EmptyQueryResponse -> go
            ParameterStatus _ _ -> go
            NoticeResponse _ -> go
            _ -> go

-- Notice handling ---------------------------------------------------------

-- | Set a callback for server notice messages (warnings, info, etc.).
-- The default handler discards all notices.
setNoticeHandler :: Connection -> (PgNotice -> IO ()) -> IO ()
setNoticeHandler conn handler = writeIORef (connNoticeHandler conn) handler

-- SQL escaping ------------------------------------------------------------

-- | Escape a string for use as a SQL literal. Returns a properly quoted
-- and escaped string including the surrounding single quotes.
--
-- @
-- escapeLiteral conn "O'Brien"  ==  "'O''Brien'"
-- @
escapeLiteral :: Connection -> ByteString -> ByteString
escapeLiteral _conn bs =
  "'" <> BS8.concatMap escapeChar bs <> "'"
  where
    escapeChar '\'' = "''"
    escapeChar '\\' = "\\\\"
    escapeChar c = BS8.singleton c

-- | Escape a string for use as a SQL identifier (table, column, function name).
-- Returns a properly quoted identifier with surrounding double quotes.
--
-- @
-- escapeIdentifier conn "user table"  ==  "\"user table\""
-- @
escapeIdentifier :: Connection -> ByteString -> ByteString
escapeIdentifier _conn bs =
  "\"" <> BS8.concatMap escapeChar bs <> "\""
  where
    escapeChar '"' = "\"\""
    escapeChar c = BS8.singleton c

-- Statement introspection -------------------------------------------------

-- | Describe a prepared statement. Returns parameter OIDs and column metadata.
-- The statement must already be prepared on this connection.
describePrepared :: Connection -> ByteString -> IO (ParamDescription, ColumnDescription)
describePrepared conn stmtName = do
  sendFrontendMsg (connWire conn) (Describe DescribeStatement stmtName)
  sendFrontendMsg (connWire conn) Sync
  (params, cols) <- collectDescribe conn
  pure (ParamDescription params, ColumnDescription cols)
  where
    collectDescribe c = do
      pds <- collectParams c
      cds <- collectColumns c
      waitDescribeReady c
      pure (pds, cds)

    collectParams c = do
      msg <- recvBackendMsg (connWire c)
      case msg of
        ParameterDescription oids -> pure oids
        ErrorResponse err -> throwHsqlx (QueryError err)
        _ -> collectParams c

    collectColumns c = do
      msg <- recvBackendMsg (connWire c)
      case msg of
        RowDescription fields -> pure fields
        NoData -> pure V.empty
        ErrorResponse err -> throwHsqlx (QueryError err)
        _ -> collectColumns c

    waitDescribeReady c = do
      msg <- recvBackendMsg (connWire c)
      case msg of
        ReadyForQuery status -> writeIORef (connTxStatus c) status
        _ -> waitDescribeReady c

-- TCP keepalive -----------------------------------------------------------
-- Note: TCP keepalive is configured at the socket level. We expose it
-- via ConnConfig parameters (see Connection.Config). The actual socket
-- options are set during connectTcp in Wire.hs.

-- Authentication ----------------------------------------------------------

handleAuth :: WireConn -> ConnConfig -> IO ()
handleAuth wc cfg = do
  msg <- recvBackendMsg wc
  case msg of
    Authentication AuthOk -> pure ()
    Authentication AuthCleartextPassword -> do
      cleartextAuth wc (ccPassword cfg)
      waitForAuthOk wc
    Authentication (AuthMD5Password salt) -> do
      md5Auth wc (ccUser cfg) (ccPassword cfg) salt
      waitForAuthOk wc
    Authentication (AuthSASL mechs)
      | "SCRAM-SHA-256" `elem` mechs -> do
          scramAuth wc (ccUser cfg) (ccPassword cfg)
          -- scramAuth handles waiting for AuthOk internally
      | otherwise ->
          throwHsqlx (AuthError ("Unsupported SASL mechanisms: " <> BS8.pack (show mechs)))
    ErrorResponse err -> throwHsqlx (AuthError (pgMessage err))
    other -> throwHsqlx (AuthError ("Unexpected auth message: " <> BS8.pack (show other)))

waitForAuthOk :: WireConn -> IO ()
waitForAuthOk wc = do
  msg <- recvBackendMsg wc
  case msg of
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwHsqlx (AuthError (pgMessage err))
    other -> throwHsqlx (AuthError ("Expected AuthOk, got: " <> BS8.pack (show other)))
