module Hsqlx.Connection
  ( Connection (..)
  , connect
  , connectString
  , close
  , withConnection
  , simpleQuery
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Int (Int32)
import Data.Vector qualified as V
import Data.Word (Word64)
import Hsqlx.Auth.Cleartext (cleartextAuth)
import Hsqlx.Auth.MD5 (md5Auth)
import Hsqlx.Auth.ScramSHA256 (scramAuth)
import Hsqlx.Connection.Config (ConnConfig (..), parseConnString)
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Protocol.Backend
import Hsqlx.Protocol.Builders (buildStartup)
import Hsqlx.Protocol.Frontend (FrontendMsg (..), StartupParams (..))
import Hsqlx.Wire (WireConn (..), connectTcp, recvBackendMsg, sendFrontendMsg, sendRawBytes)

-- | A connection to a PostgreSQL database.
data Connection = Connection
  { connWire :: WireConn
  , connConfig :: ConnConfig
  , connParams :: IORef (Map ByteString ByteString)
  , connBackendPid :: Int32
  , connBackendKey :: Int32
  , connTxStatus :: IORef TxStatus
  , connStmtCache :: IORef (Map ByteString ByteString)
  , connStmtCounter :: IORef Word64
  }

-- | Connect using a 'ConnConfig'.
connect :: ConnConfig -> IO Connection
connect cfg = do
  wc <- connectTcp (ccHost cfg) (ccPort cfg)

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
