-- | Query execution functions using the PostgreSQL extended query protocol.
--
-- All functions use binary format for both parameters and results,
-- prepared statement caching, and message coalescing for minimal
-- round-trip overhead.
--
-- When a statement hasn't been prepared yet, Parse is coalesced with
-- Bind+Execute+Sync into a single round-trip (the asyncpg technique).
-- Subsequent executions skip Parse entirely.
--
-- For large batches (>256 items), 'executeBatch' streams Bind+Execute
-- pairs in chunks via exclusive mode to bound memory usage.
module Hsqlx.Execute
  ( -- * Queries
    fetchOne
  , fetchAll
  , fetchScalar
    -- * Commands
  , execute
    -- * Pipelined batch execution
  , executeBatch
    -- * Pipelined batch reads
  , fetchBatchOne
  , fetchBatchAll
    -- * Internal (used by other Hsqlx modules)
  , ensurePrepared
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Async (Request (..), Response (..), ResponseCollector (..), submitRequest, submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Protocol.Oid qualified as Oid
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import Hsqlx.Statement (Statement (..))

------------------------------------------------------------------------
-- Shared constants (avoid per-call allocation)
------------------------------------------------------------------------

-- | @[BinaryFormat]@ — used for both parameter and result format codes.
binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Fetch zero or one row. Returns 'Nothing' if the query produces no results.
fetchOne :: Connection -> Statement p r -> p -> IO (Maybe r)
fetchOne conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  case rows of
    [] -> pure Nothing
    (row : _) -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)

-- | Fetch all result rows as a list.
--
-- For large result sets, consider 'Hsqlx.Streaming.withCursor' instead.
fetchAll :: Connection -> Statement p r -> p -> IO [r]
fetchAll conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  decodeRows (stmtDecode stmt) rows

-- | Fetch a single scalar value. Throws 'DecodeError' if the query
-- returns zero or more than one row.
fetchScalar :: Connection -> Statement p r -> p -> IO r
fetchScalar conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  case rows of
    [row] -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val
    [] -> throwHsqlx (DecodeError "fetchScalar: query returned no rows")
    _ -> throwHsqlx (DecodeError "fetchScalar: query returned more than one row")

------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------

-- | Execute a command (INSERT\/UPDATE\/DELETE). Returns the number of
-- rows affected.
execute :: Connection -> Statement p () -> p -> IO Int64
execute conn stmt params = do
  (name, needsParse) <- lookupOrAllocStmt conn stmt
  let encodedParams = stmtEncode stmt params
      bindExec =
        [ Bind "" name binaryFmtVec encodedParams V.empty
        , Execute "" 0
        , Sync
        ]
      msgs = if needsParse
        then Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)) : bindExec
        else bindExec
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs CollectCommand
  case resp of
    RespCommand tag -> do
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      pure (tagRows tag)
    _ -> throwHsqlx (ProtocolError "execute: unexpected response type")

-- | Execute a batch of commands using pipeline mode.
--
-- Sends all Bind+Execute messages with a single Sync at the end,
-- eliminating per-row round-trip overhead. Returns total rows affected.
--
-- For batches larger than 256 items, switches to streaming mode
-- (exclusive wire access, chunked sends) to bound memory usage.
executeBatch :: Connection -> Statement p () -> [p] -> IO Int64
executeBatch _ _ [] = pure 0
executeBatch conn stmt paramsList = do
  (name, needsParse) <- lookupOrAllocStmt conn stmt
  case splitAtEnd batchStreamThreshold paramsList of
    -- Small batch: coalesce through async channel
    (small, Nothing) -> do
      let encode = stmtEncode stmt
          bindExecs = concatMap (\p ->
            [ Bind "" name binaryFmtVec (encode p) V.empty
            , Execute "" 0
            ]) small ++ [Sync]
          msgs = if needsParse
            then Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)) : bindExecs
            else bindExecs
      resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs (CollectBatchCommand (length small))
      case resp of
        RespBatchCommand total -> do
          when needsParse $ cacheStmt conn (stmtSQL stmt) name
          pure total
        _ -> throwHsqlx (ProtocolError "executeBatch: unexpected response type")

    -- Large batch: stream in chunks via exclusive mode
    (_, Just _) -> submitExclusive (connAsync conn) $ \wc txRef -> do
      when needsParse $ do
        sendFrontendMsgs wc [Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)), Flush]
        waitParseWire wc
      streamBatchChunks wc name stmt paramsList
      sendFrontendMsg wc Sync
      total <- collectBatchCmdWire wc txRef (length paramsList)
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      pure total

-- | Fetch zero or one row for each parameter set, pipelined.
fetchBatchOne :: Connection -> Statement p r -> [p] -> IO [Maybe r]
fetchBatchOne _ _ [] = pure []
fetchBatchOne conn stmt paramsList = do
  (name, needsParse) <- lookupOrAllocStmt conn stmt
  let encode = stmtEncode stmt
      bindExecs = concatMap (\p ->
        [ Bind "" name binaryFmtVec (encode p) binaryFmtVec
        , Execute "" 0
        ]) paramsList ++ [Sync]
      msgs = if needsParse
        then Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)) : bindExecs
        else bindExecs
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs (CollectBatch (length paramsList))
  case resp of
    RespBatchRows results -> do
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      mapM (\rows -> case rows of
        [] -> pure Nothing
        (row : _) -> case stmtDecode stmt row of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right val -> pure (Just val)) results
    _ -> throwHsqlx (ProtocolError "fetchBatchOne: unexpected response type")

-- | Fetch all rows for each parameter set, pipelined.
fetchBatchAll :: Connection -> Statement p r -> [p] -> IO [[r]]
fetchBatchAll _ _ [] = pure []
fetchBatchAll conn stmt paramsList = do
  (name, needsParse) <- lookupOrAllocStmt conn stmt
  let encode = stmtEncode stmt
      bindExecs = concatMap (\p ->
        [ Bind "" name binaryFmtVec (encode p) binaryFmtVec
        , Execute "" 0
        ]) paramsList ++ [Sync]
      msgs = if needsParse
        then Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)) : bindExecs
        else bindExecs
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs (CollectBatch (length paramsList))
  case resp of
    RespBatchRows results -> do
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      mapM (decodeRows (stmtDecode stmt)) results
    _ -> throwHsqlx (ProtocolError "fetchBatchAll: unexpected response type")

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

-- | Fetch raw rows, coalescing Parse+Bind+Execute for cache misses.
fetchRowsRaw :: Connection -> Statement p r -> p -> IO [Vector (Maybe ByteString)]
fetchRowsRaw conn stmt params = do
  (name, needsParse) <- lookupOrAllocStmt conn stmt
  let encodedParams = stmtEncode stmt params
      bindExec =
        [ Bind "" name binaryFmtVec encodedParams binaryFmtVec
        , Execute "" 0
        , Sync
        ]
      msgs = if needsParse
        then Parse name (stmtSQL stmt) (V.map Oid.unOid (stmtParamOids stmt)) : bindExec
        else bindExec
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs CollectRows
  case resp of
    RespRows rows -> do
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      pure rows
    _ -> throwHsqlx (ProtocolError "fetchRows: unexpected response type")

-- | Check the statement cache. On miss, allocate a name and evict if needed.
lookupOrAllocStmt :: Connection -> Statement p r -> IO (ByteString, Bool)
lookupOrAllocStmt conn stmt = do
  cache <- readIORef (connStmtCache conn)
  let sql = stmtSQL stmt
  case Map.lookup sql cache of
    Just name -> pure (name, False)
    Nothing -> do
      evictIfNeeded conn cache
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
      pure (name, True)

-- | Cache a statement name after successful Parse.
cacheStmt :: Connection -> ByteString -> ByteString -> IO ()
cacheStmt conn sql name =
  modifyIORef' (connStmtCache conn) (Map.insert sql name)

-- | Decode a list of raw row vectors into typed values.
decodeRows :: (Vector (Maybe ByteString) -> Either String r) -> [Vector (Maybe ByteString)] -> IO [r]
decodeRows decode = mapM $ \row -> case decode row of
  Left err -> throwHsqlx (DecodeError (BS8.pack err))
  Right !val -> pure val

-- | Maximum number of prepared statements cached per connection.
maxCachedStatements :: Int
maxCachedStatements = 256

-- | Batch size threshold for switching to streaming mode.
batchStreamThreshold :: Int
batchStreamThreshold = 256

-- | Ensure a statement is prepared on this connection. Returns the statement name.
-- Uses Parse+Flush (no ReadyForQuery overhead). Prefer the coalesced path in
-- 'fetchRowsRaw' / 'execute' for better latency; this function exists for
-- callers that need the statement name before building messages (Pipeline, Fold).
ensurePrepared :: Connection -> Statement p r -> IO ByteString
ensurePrepared conn stmt = do
  cache <- readIORef (connStmtCache conn)
  let sql = stmtSQL stmt
  case Map.lookup sql cache of
    Just name -> pure name
    Nothing -> do
      evictIfNeeded conn cache
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
          oids = V.map Oid.unOid (stmtParamOids stmt)
      resp <- submitRequest (connAsync conn) $ ReqPrepare (Parse name sql oids)
      case resp of
        RespParsed -> do
          modifyIORef' (connStmtCache conn) (Map.insert sql name)
          pure name
        _ -> throwHsqlx (ProtocolError "ensurePrepared: unexpected response type")

-- | If the cache has reached its limit, close the oldest prepared statement.
evictIfNeeded :: Connection -> Map ByteString ByteString -> IO ()
evictIfNeeded conn cache
  | Map.size cache < maxCachedStatements = pure ()
  | otherwise = case Map.lookupMin cache of
      Nothing -> pure ()
      Just (oldSql, oldName) -> do
        resp <- submitRequest (connAsync conn) $ ReqClose (Close DescribeStatement oldName)
        case resp of
          RespClosed -> pure ()
          _ -> pure () -- best effort
        modifyIORef' (connStmtCache conn) (Map.delete oldSql)

------------------------------------------------------------------------
-- Streaming batch helpers (exclusive mode, direct wire access)
------------------------------------------------------------------------

-- | Stream Bind+Execute pairs in chunks of 'batchStreamThreshold'.
streamBatchChunks :: WireConn -> ByteString -> Statement p () -> [p] -> IO ()
streamBatchChunks _ _ _ [] = pure ()
streamBatchChunks wc name stmt paramsList = do
  let (chunk, rest) = splitAt batchStreamThreshold paramsList
      msgs = concatMap (\params ->
        let ep = stmtEncode stmt params
         in [ Bind "" name binaryFmtVec ep V.empty
            , Execute "" 0
            ]) chunk
  sendFrontendMsgs wc msgs
  streamBatchChunks wc name stmt rest

-- | Wait for ParseComplete on the wire (used after Flush in exclusive mode).
waitParseWire :: WireConn -> IO ()
waitParseWire wc = do
  msg <- recvBackendMsg wc
  case msg of
    ParseComplete -> pure ()
    NoticeResponse _ -> waitParseWire wc
    ErrorResponse err -> throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected ParseComplete, got: " <> BS8.pack (show other)))

-- | Collect batch command results on the wire (used in streaming mode).
collectBatchCmdWire :: WireConn -> IORef TxStatus -> Int -> IO Int64
collectBatchCmdWire wc txRef = go 0
  where
    go !total 0 = do
      waitReadyWire wc txRef
      pure total
    go !total !n = do
      msg <- recvBackendMsg wc
      case msg of
        BindComplete -> go total n
        CommandComplete tag -> go (total + tagRows tag) (n - 1)
        ErrorResponse err -> do
          waitReadyWire wc txRef
          throwHsqlx (QueryError err)
        NoticeResponse _ -> go total n
        other -> throwHsqlx (ProtocolError ("Unexpected in batch: " <> BS8.pack (show other)))

-- | Wait for ReadyForQuery on the wire.
waitReadyWire :: WireConn -> IORef TxStatus -> IO ()
waitReadyWire wc txRef = do
  msg <- recvBackendMsg wc
  case msg of
    ReadyForQuery status -> writeIORef txRef status
    _ -> waitReadyWire wc txRef

-- | Split a list, returning (prefix, Nothing) if length <= n,
-- or (full list, Just ()) if length > n. Avoids forcing the full spine
-- for the common small-batch case.
splitAtEnd :: Int -> [a] -> ([a], Maybe ())
splitAtEnd _ [] = ([], Nothing)
splitAtEnd 0 xs = (xs, Just ())
splitAtEnd n (x : xs) = case splitAtEnd (n - 1) xs of
  (rest, flag) -> (x : rest, flag)

tagRows :: CommandTag -> Int64
tagRows (InsertTag n) = n
tagRows (UpdateTag n) = n
tagRows (DeleteTag n) = n
tagRows (SelectTag n) = n
tagRows (OtherTag _) = 0
