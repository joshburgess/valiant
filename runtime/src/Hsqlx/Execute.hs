module Hsqlx.Execute
  ( fetchOne
  , fetchAll
  , fetchScalar
  , execute
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hsqlx.Connection (Connection (..))
import Hsqlx.Protocol.Oid qualified as Oid
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Protocol.Backend
import Hsqlx.Protocol.Frontend
import Hsqlx.Statement (Statement (..))
import Hsqlx.Wire (recvBackendMsg, sendFrontendMsg)

-- | Fetch zero or one row.
fetchOne :: Connection -> Statement p r -> p -> IO (Maybe r)
fetchOne conn stmt params = do
  rows <- executeExtended conn stmt params
  case rows of
    [] -> pure Nothing
    (row : _) -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)

-- | Fetch all rows.
fetchAll :: Connection -> Statement p r -> p -> IO [r]
fetchAll conn stmt params = do
  rows <- executeExtended conn stmt params
  mapM decodeRow rows
  where
    decodeRow row = case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val

-- | Fetch a single scalar value.
fetchScalar :: Connection -> Statement p r -> p -> IO r
fetchScalar conn stmt params = do
  rows <- executeExtended conn stmt params
  case rows of
    [row] -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val
    [] -> throwHsqlx (DecodeError "fetchScalar: query returned no rows")
    _ -> throwHsqlx (DecodeError "fetchScalar: query returned more than one row")

-- | Execute a command (INSERT/UPDATE/DELETE). Returns rows affected.
execute :: Connection -> Statement p () -> p -> IO Int64
execute conn stmt params = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params

  -- Bind + Execute + Sync
  sendFrontendMsg (connWire conn) $
    Bind
      ""
      stmtName
      (V.singleton BinaryFormat) -- all params in binary
      encodedParams
      (V.empty) -- no result columns for commands
  sendFrontendMsg (connWire conn) (Execute "" 0)
  sendFrontendMsg (connWire conn) Sync

  -- Collect responses
  rowsAffected <- collectCommandResult conn
  pure rowsAffected

-- Extended query protocol -------------------------------------------------

executeExtended :: Connection -> Statement p r -> p -> IO [Vector (Maybe ByteString)]
executeExtended conn stmt params = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params

  -- Bind + Execute + Sync
  -- Use a single format code = BinaryFormat, which PG applies to all columns
  sendFrontendMsg (connWire conn) $
    Bind
      ""
      stmtName
      (V.singleton BinaryFormat) -- all params in binary
      encodedParams
      (V.singleton BinaryFormat) -- all results in binary
  sendFrontendMsg (connWire conn) (Execute "" 0)
  sendFrontendMsg (connWire conn) Sync

  -- Collect rows
  collectRows conn

-- | Ensure a statement is prepared on this connection. Returns the statement name.
ensurePrepared :: Connection -> Statement p r -> IO ByteString
ensurePrepared conn stmt = do
  cache <- readIORef (connStmtCache conn)
  let sql = stmtSQL stmt
  case Map.lookup sql cache of
    Just name -> pure name
    Nothing -> do
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
          oids = V.map Oid.unOid (stmtParamOids stmt)
      sendFrontendMsg (connWire conn) (Parse name sql oids)
      sendFrontendMsg (connWire conn) Sync

      -- Wait for ParseComplete + ReadyForQuery
      waitParseComplete conn
      modifyIORef' (connStmtCache conn) (Map.insert sql name)
      pure name

collectRows :: Connection -> IO [Vector (Maybe ByteString)]
collectRows conn = go []
  where
    go acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (reverse acc)
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in query: " <> BS8.pack (show other)))

collectCommandResult :: Connection -> IO Int64
collectCommandResult conn = go 0
  where
    go n = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go n
        CommandComplete tag -> go (tagRows tag)
        EmptyQueryResponse -> go n
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure n
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go n
        other -> throwHsqlx (ProtocolError ("Unexpected in execute: " <> BS8.pack (show other)))

    tagRows (InsertTag n) = n
    tagRows (UpdateTag n) = n
    tagRows (DeleteTag n) = n
    tagRows (SelectTag n) = n
    tagRows (OtherTag _) = 0

waitParseComplete :: Connection -> IO ()
waitParseComplete conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ParseComplete -> waitReady conn
    ErrorResponse err -> do
      -- Drain until ReadyForQuery
      waitReady conn
      throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected ParseComplete, got: " <> BS8.pack (show other)))

waitReady :: Connection -> IO ()
waitReady conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ReadyForQuery status -> writeIORef (connTxStatus conn) status
    _ -> waitReady conn
