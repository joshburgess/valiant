-- | Streaming query results using server-side cursors.
--
-- Instead of fetching all rows at once, a cursor fetches rows in batches,
-- allowing processing of large result sets without loading everything into
-- memory.
module Hsqlx.Streaming
  ( withCursor
  , fetchBatch
  , CursorState (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector (Vector)
import Hsqlx.Connection (Connection (..), simpleQuery)
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Protocol.Backend
import Hsqlx.Protocol.Frontend
import Hsqlx.Statement (Statement (..))
import Hsqlx.Wire (recvBackendMsg, sendFrontendMsg)

-- | State of an open cursor.
data CursorState = CursorState
  { csName :: ByteString
  , csConn :: Connection
  , csExhausted :: IORef Bool
  }

-- | Open a cursor for a statement, run an action with it, then close.
-- Must be called inside a transaction.
withCursor
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size (rows per fetch)
  -> (CursorState -> IO a)
  -> IO a
withCursor conn stmt _params _batchSize action = do
  let cursorName = "hsqlx_cursor"
      declareSql =
        "DECLARE "
          <> cursorName
          <> " NO SCROLL CURSOR FOR "
          <> stmtSQL stmt

  -- We use the simple query protocol for DECLARE since we need to
  -- embed the SQL directly. The parameters are bound via a subselect.
  -- For simplicity, we declare without params and require the caller
  -- to use a parameterless statement (the typical cursor pattern).
  _ <- simpleQuery conn declareSql

  exhausted <- newIORef False
  let cs = CursorState cursorName conn exhausted
  result <- action cs

  -- Close the cursor
  _ <- simpleQuery conn ("CLOSE " <> cursorName)
  pure result

-- | Fetch the next batch of rows from a cursor. Returns an empty list when
-- the cursor is exhausted.
fetchBatch :: CursorState -> Int -> IO [Vector (Maybe ByteString)]
fetchBatch cs n = do
  done <- readIORef (csExhausted cs)
  if done
    then pure []
    else do
      let fetchSql = "FETCH FORWARD " <> BS8.pack (show n) <> " FROM " <> csName cs
      sendFrontendMsg (connWire (csConn cs)) (Query fetchSql)
      rows <- collectFetchResults (csConn cs)
      if null rows
        then do
          writeIORef (csExhausted cs) True
          pure []
        else pure rows

collectFetchResults :: Connection -> IO [Vector (Maybe ByteString)]
collectFetchResults conn = go []
  where
    go acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        RowDescription _ -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (reverse acc)
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in cursor fetch: " <> BS8.pack (show other)))
