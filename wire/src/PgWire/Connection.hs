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
    -- * Server ping
  , ping
    -- * Password utilities
  , lookupPgpass
  , encryptPassword
    -- * Protocol tracing
  , setTraceHandler
  ) where

import Control.Exception (SomeException, bracket, try)
import Crypto.Hash qualified
import Crypto.Hash (MD5, hash, Digest)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
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
import PgWire.Connection.Config (ConnConfig (..), TargetSessionAttrs (..), TlsMode (..), parseConnString, parseHosts)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Builders (buildStartup)
import PgWire.Protocol.Frontend (DescribeTarget (..), FrontendMsg (..), StartupParams (..))
import PgWire.Wire (TlsConfig (..), TraceDirection (..), WireConn (..), connectTcpTimeout, recvBackendMsg, sendFrontendMsg, sendRawBytes, upgradeTls)

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

-- | Connect using a 'ConnConfig'. Supports multi-host failover:
-- if @ccHost@ contains comma-separated hosts (@\"host1,host2\"@),
-- tries each in order until one succeeds. If @ccTargetSessionAttrs@
-- is set, verifies the server matches (e.g., primary vs standby).
connect :: ConnConfig -> IO Connection
connect cfg0 = do
  -- Auto-lookup password from ~/.pgpass if not provided
  cfg <- if BS8.null (ccPassword cfg0)
    then do
      mPw <- lookupPgpass cfg0
      pure $ case mPw of
        Just pw -> cfg0 { ccPassword = pw }
        Nothing -> cfg0
    else pure cfg0
  let hosts = parseHosts (ccHost cfg)
  orderedHosts <- if ccLoadBalanceHosts cfg
    then shuffleHosts hosts
    else pure hosts
  tryHosts cfg orderedHosts

tryHosts :: ConnConfig -> [String] -> IO Connection
tryHosts _ [] = throwHsqlx (ConnectionError "All hosts failed")
tryHosts cfg [h] = connectSingleHost cfg { ccHost = h }
tryHosts cfg (h : hs) = do
  result <- try @SomeException (connectSingleHost cfg { ccHost = h })
  case result of
    Right conn -> do
      ok <- checkSessionAttrs conn (ccTargetSessionAttrs cfg)
      if ok
        then pure conn
        else do
          close conn
          tryHosts cfg hs
    Left _ -> tryHosts cfg hs

checkSessionAttrs :: Connection -> TargetSessionAttrs -> IO Bool
checkSessionAttrs _ SessionAny = pure True
checkSessionAttrs conn attr = do
  (rows, _) <- simpleQuery conn "SHOW transaction_read_only"
  case rows of
    [[Just val]] ->
      let readOnly = val == "on"
       in pure $ case attr of
            SessionReadWrite -> not readOnly
            SessionReadOnly -> readOnly
            SessionPrimary -> not readOnly
            SessionStandby -> readOnly
            SessionPreferStandby -> True
            SessionAny -> True
    _ -> pure True

-- | Fisher-Yates shuffle for load balancing
shuffleHosts :: [a] -> IO [a]
shuffleHosts [] = pure []
shuffleHosts [x] = pure [x]
shuffleHosts xs = do
  let arr = zip [0 :: Int ..] xs
      n = length xs
  go (reverse arr) (n - 1)
  where
    go [] _ = pure []
    go ((_, x) : rest) 0 = pure [x]
    go items i = do
      j <- randomRIO (0, i)
      let picked = snd (items !! j)
          remaining = take j items ++ drop (j + 1) items
      (picked :) <$> go remaining (i - 1)

connectSingleHost :: ConnConfig -> IO Connection
connectSingleHost cfg = do
  wc0 <- connectTcpTimeout (ccConnectTimeout cfg) (ccHost cfg) (ccPort cfg)

  let tlsCfg = TlsConfig
        { tlsHostname = ccHost cfg
        , tlsVerify = ccTls cfg `elem` [TlsVerifyCa, TlsVerifyFull]
        , tlsVerifyHostname = ccTls cfg == TlsVerifyFull
        , tlsClientCert = ccSslCert cfg
        , tlsClientKey = ccSslKey cfg
        , tlsCaCert = ccSslRootCert cfg
        }

  -- Optionally upgrade to TLS
  wc <- case ccTls cfg of
    TlsDisable -> pure wc0
    TlsRequire -> upgradeTls wc0 tlsCfg
    TlsVerifyCa -> upgradeTls wc0 tlsCfg
    TlsVerifyFull -> upgradeTls wc0 tlsCfg
    TlsPrefer -> do
      result <- try @SomeException (upgradeTls wc0 tlsCfg)
      case result of
        Right tlsWc -> pure tlsWc
        Left _ -> do
          wcClose wc0
          connectTcpTimeout (ccConnectTimeout cfg) (ccHost cfg) (ccPort cfg)

  -- Send startup message
  let extraParams =
        [ ("client_encoding", ccClientEncoding cfg) | ccClientEncoding cfg /= "UTF8" ]
        ++ [ ("options", ccOptions cfg) | not (BS8.null (ccOptions cfg)) ]
      startup =
        StartupParams
          { spUser = ccUser cfg
          , spDatabase = ccDatabase cfg
          , spAppName = ccAppName cfg
          , spExtraParams = extraParams
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

-- Server ping -------------------------------------------------------------

-- | Check if a PostgreSQL server is accepting connections, without
-- fully authenticating. Attempts a TCP connection and checks if the
-- server responds to the startup sequence.
ping :: ConnConfig -> IO Bool
ping cfg = do
  result <- try @SomeException (connect cfg >>= close)
  pure $ case result of
    Right () -> True
    Left _ -> False

-- Password file -----------------------------------------------------------

-- | Look up a password from @~/.pgpass@ file.
-- Format: @hostname:port:database:username:password@ (one per line).
-- @*@ matches any value in a field.
lookupPgpass :: ConnConfig -> IO (Maybe ByteString)
lookupPgpass cfg = do
  home <- lookupEnv "HOME"
  case home of
    Nothing -> pure Nothing
    Just h -> do
      let path = h <> "/.pgpass"
      exists <- doesFileExist path
      if not exists
        then pure Nothing
        else do
          contents <- BS8.readFile path
          let host = BS8.pack (ccHost cfg)
              port = BS8.pack (show (ccPort cfg))
              db = ccDatabase cfg
              user = ccUser cfg
          pure (findMatch host port db user (BS8.lines contents))
  where
    findMatch _ _ _ _ [] = Nothing
    findMatch host port db user (line : rest)
      | BS8.null line || BS8.head line == '#' = findMatch host port db user rest
      | otherwise = case BS8.split ':' line of
          [h, p, d, u, pw]
            | matches h host && matches p port && matches d db && matches u user ->
                Just pw
          _ -> findMatch host port db user rest
    matches "*" _ = True
    matches pattern value = pattern == value

-- Password encryption -----------------------------------------------------

-- | Encrypt a password for storage, using the same algorithm as
-- @PQencryptPasswordConn@. Supports MD5 format.
encryptPassword :: ByteString -> ByteString -> ByteString
encryptPassword user password =
  -- MD5 format: "md5" + hex(md5(password + user))
  let digest = BA.convert (hash (password <> user) :: Digest MD5) :: ByteString
   in "md5" <> toHex digest

toHex :: ByteString -> ByteString
toHex bs = BS8.pack (concatMap byteToHex (BS.unpack bs))
  where
    byteToHex w =
      let (hi, lo) = w `divMod` 16
       in [hexDigit hi, hexDigit lo]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

-- Protocol tracing --------------------------------------------------------

-- | Set a trace callback for debugging protocol messages.
-- The callback receives the direction (True = send, False = recv)
-- and the raw bytes.
-- | Enable protocol tracing on this connection.
-- The callback receives 'True' for send, 'False' for recv, plus the raw bytes.
-- Set to @Nothing@ to disable tracing.
--
-- @
-- setTraceHandler conn $ \\isSend bytes ->
--   BS8.putStrLn $ (if isSend then \">>> \" else \"<<< \") <> BS8.take 40 bytes
-- @
setTraceHandler :: Connection -> (Bool -> ByteString -> IO ()) -> IO ()
setTraceHandler conn handler =
  writeIORef (wcTrace (connWire conn)) (Just wrapper)
  where
    wrapper TraceSend bs = handler True bs
    wrapper TraceRecv bs = handler False bs

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
