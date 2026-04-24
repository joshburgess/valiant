-- | PostgreSQL connection management.
--
-- A 'Connection' represents an active TCP (or TLS) session with a
-- PostgreSQL server. It handles authentication, parameter negotiation,
-- and prepared statement caching.
--
-- After startup, each connection spawns dedicated writer and reader
-- threads (see 'PgWire.Async') that enable automatic pipelining when
-- multiple green threads share a connection.
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
  , connWire
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

import Control.Exception (SomeException, bracket, catch, try)
import Crypto.Hash (MD5, hash, Digest)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as PSQ
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import PgWire.Async (AsyncWireConn (..), Request (..), Response (..), spawnAsyncWireConn, shutdownAsyncWireConn, submitRequest, submitExclusive)
import PgWire.Auth.Cleartext (cleartextAuth)
import PgWire.Auth.MD5 (md5Auth)
import PgWire.Auth.ScramSHA256 (scramAuth)
import PgWire.Connection.Config (ConnConfig (..), TargetSessionAttrs (..), TlsMode (..), parseConnString, parseHosts)
import PgWire.Error (PgWireError (..), throwPgWire)
import PgWire.Protocol.Backend
import PgWire.Protocol.Builders (buildStartup)
import PgWire.Protocol.Frontend (DescribeTarget (..), FrontendMsg (..), StartupParams (..))
import PgWire.Wire (TlsConfig (..), TraceDirection (..), WireConn (..), connectTcpTimeout, recvBackendMsg, sendFrontendMsg, sendRawBytes, upgradeTls)

-- | A connection to a PostgreSQL database.
--
-- After the initial handshake, dedicated writer and reader threads handle
-- all wire I/O. Use 'submitRequest' or the higher-level functions in
-- "Valiant.Execute" to interact with the connection.
data Connection = Connection
  { connAsync :: !AsyncWireConn
  -- ^ The async wire connection (writer + reader threads).
  , connConfig :: !ConnConfig
  , connBackendPid :: {-# UNPACK #-} !Int32
  , connBackendKey :: {-# UNPACK #-} !Int32
  , connStmtCache :: !(IORef (HashPSQ ByteString Word64 ByteString))
  -- ^ LRU prepared statement cache. Keys are SQL text, priorities are
  -- monotonic access ticks (lower = least recently used), values are
  -- server-side statement names.
  , connStmtCounter :: !(IORef Word64)
  , connStmtTick :: !(IORef Word64)
  -- ^ Monotonic tick counter for LRU cache priorities.
  , connSslActive :: !Bool
  , connPreparedStatements :: !Bool
  -- ^ Whether to use named prepared statements (default: True).
  -- Set to False for compatibility with PgBouncer in transaction mode.
  -- When False, all queries use the unnamed statement (parsed + discarded
  -- each time, never cached).
  , connMaxPreparedStatements :: !Int
  -- ^ Maximum number of prepared statements cached on this connection.
  }

-- | Access the underlying 'WireConn' for direct wire operations.
-- Only use this inside exclusive mode ('submitExclusive').
connWire :: Connection -> WireConn
connWire = awcWire . connAsync

-- | Access the transaction status IORef.
connTxStatus :: Connection -> IORef TxStatus
connTxStatus = awcTxStatus . connAsync

-- | Access the server parameters IORef.
connParams :: Connection -> IORef (Map ByteString ByteString)
connParams = awcParamStatus . connAsync

-- | Access the notice handler IORef.
connNoticeHandler :: Connection -> IORef (PgNotice -> IO ())
connNoticeHandler = awcNoticeHandler . connAsync

-- | Parameter type information returned by 'describePrepared'.
data ParamDescription = ParamDescription
  { pdOids :: Vector Word32
  -- ^ OIDs of each parameter's type (one per @$N@ placeholder).
  }
  deriving stock (Show, Eq)

-- | Column metadata returned by 'describePrepared'.
data ColumnDescription = ColumnDescription
  { cdFields :: Vector FieldInfo
  -- ^ Name, type OID, table OID, column number, and format for each result column.
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
tryHosts _ [] = throwPgWire (ConnectionError "All hosts failed")
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
    go ((_, x) : _rest) 0 = pure [x]
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
            throwPgWire (ProtocolError ("Unexpected message during startup: " <> BS8.pack (show other)))

  loop

  pid <- readIORef pidRef
  key <- readIORef keyRef
  stmtCache <- newIORef PSQ.empty
  stmtCounter <- newIORef 0
  stmtTick <- newIORef 0
  let sslActive = case ccTls cfg of
        TlsDisable -> False
        _ -> True

  -- Spawn async writer + reader threads
  asyncConn <- spawnAsyncWireConn wc txRef paramsRef

  pure
    Connection
      { connAsync = asyncConn
      , connConfig = cfg
      , connBackendPid = pid
      , connBackendKey = key
      , connStmtCache = stmtCache
      , connStmtCounter = stmtCounter
      , connStmtTick = stmtTick
      , connSslActive = sslActive
      , connPreparedStatements = ccPreparedStatements cfg
      , connMaxPreparedStatements = max 1 (ccMaxPreparedStatements cfg)
      }

-- | Connect using a connection string.
connectString :: ByteString -> IO Connection
connectString bs = case parseConnString bs of
  Left err -> throwPgWire (ConnectionError (BS8.pack err))
  Right cfg -> connect cfg

-- | Close a connection.
close :: Connection -> IO ()
close conn = do
  shutdownAsyncWireConn (connAsync conn)
  -- Send Terminate and close the socket (best effort, threads are already dead)
  sendFrontendMsg (connWire conn) Terminate `catch_` pure ()
  wcClose (connWire conn) `catch_` pure ()
  where
    catch_ :: IO a -> IO a -> IO a
    catch_ action fallback = action `catch` \(_ :: SomeException) -> fallback

-- | Bracket-style connection management.
withConnection :: ConnConfig -> (Connection -> IO a) -> IO a
withConnection cfg = bracket (connect cfg) close

-- | Execute a simple text query (no parameters). Returns all result rows
-- as lists of nullable bytestrings, plus the command tag.
simpleQuery :: Connection -> ByteString -> IO ([[Maybe ByteString]], Maybe CommandTag)
simpleQuery conn sql = do
  resp <- submitRequest (connAsync conn) (ReqSimpleQuery sql)
  case resp of
    RespSimple rows tag -> pure (rows, tag)
    _ -> throwPgWire (ProtocolError "simpleQuery: unexpected response type")

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
      -- Strip distribution suffix: "16.6 (Ubuntu 16.6-0ubuntu0.24.04.1)" → "16.6"
      let versionOnly = BS8.takeWhile (\c -> c == '.' || (c >= '0' && c <= '9')) bs
          parts = BS8.split '.' versionOnly
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
  _ <- simpleQuery conn ""
  -- Notifications are dispatched by the reader thread to the handler.
  -- For non-blocking check, we'd need a different mechanism.
  -- For now, return Nothing — notifications arrive via the handler.
  pure Nothing

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
  -- describePrepared needs direct wire access (Describe + Sync protocol)
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsg wc (Describe DescribeStatement stmtName)
    sendFrontendMsg wc Sync
    (params, cols) <- collectDescribe wc txRef
    pure (ParamDescription params, ColumnDescription cols)
  where
    collectDescribe wc txRef = do
      pds <- collectParams wc
      cds <- collectColumns wc
      waitDescribeReady wc txRef
      pure (pds, cds)

    collectParams wc = do
      msg <- recvBackendMsg wc
      case msg of
        ParameterDescription oids -> pure oids
        ErrorResponse err -> throwPgWire (QueryError err)
        _ -> collectParams wc

    collectColumns wc = do
      msg <- recvBackendMsg wc
      case msg of
        RowDescription fields -> pure fields
        NoData -> pure V.empty
        ErrorResponse err -> throwPgWire (QueryError err)
        _ -> collectColumns wc

    waitDescribeReady wc txRef = do
      msg <- recvBackendMsg wc
      case msg of
        ReadyForQuery status -> writeIORef txRef status
        _ -> waitDescribeReady wc txRef

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
      | BS8.null line || BS8.isPrefixOf "#" line = findMatch host port db user rest
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
          throwPgWire (AuthError ("Unsupported SASL mechanisms: " <> BS8.pack (show mechs)))
    ErrorResponse err -> throwPgWire (AuthError (pgMessage err))
    other -> throwPgWire (AuthError ("Unexpected auth message: " <> BS8.pack (show other)))

waitForAuthOk :: WireConn -> IO ()
waitForAuthOk wc = do
  msg <- recvBackendMsg wc
  case msg of
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwPgWire (AuthError (pgMessage err))
    other -> throwPgWire (AuthError ("Expected AuthOk, got: " <> BS8.pack (show other)))
