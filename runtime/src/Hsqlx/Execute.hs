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
  , fetchAllWith
  , fetchScalar
  , fetchOneOrThrow
  , fetchExists
    -- * Commands
  , execute
  , executeReturning
  , executeMany
    -- * Raw (unchecked) queries
  , rawFetchAll
  , rawFetchOne
  , rawExecute
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
import Data.Word (Word32)
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

-- | Fetch zero or one row from a typed 'Statement'.
-- Returns 'Nothing' if the query produces no results; if multiple rows are
-- returned, only the first is used.
--
-- @
-- mUser <- fetchOne conn findById 42
-- @
fetchOne :: Connection -> Statement p r -> p -> IO (Maybe r)
fetchOne conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  case rows of
    [] -> pure Nothing
    (row : _) -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)

-- | Fetch all result rows as a list.
-- Decodes every row eagerly; for large result sets consider streaming instead.
--
-- @
-- users <- fetchAll conn listAllUsers ()
-- @
fetchAll :: Connection -> Statement p r -> p -> IO [r]
fetchAll conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  decodeRows (stmtDecode stmt) rows

-- | Fetch exactly one row and decode it as a scalar value.
-- Throws 'DecodeError' if the query returns zero rows or more than one row.
--
-- @
-- n <- fetchScalar conn countUsers ()
-- @
fetchScalar :: Connection -> Statement p r -> p -> IO r
fetchScalar conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  case rows of
    [row] -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val
    [] -> throwHsqlx (DecodeError "fetchScalar: query returned no rows")
    _ -> throwHsqlx (DecodeError "fetchScalar: query returned more than one row")

-- | Like 'fetchOne' but throws 'DecodeError' if no rows are returned.
-- Useful when you know the row must exist (e.g., fetching by primary key
-- after a successful INSERT...RETURNING).
--
-- @
-- user <- fetchOneOrThrow conn findById userId
-- @
fetchOneOrThrow :: Connection -> Statement p r -> p -> IO r
fetchOneOrThrow conn stmt params = do
  mResult <- fetchOne conn stmt params
  case mResult of
    Nothing -> throwHsqlx (DecodeError "fetchOneOrThrow: query returned no rows")
    Just val -> pure val

-- | Check whether a query returns any rows. Useful for @EXISTS@-style queries.
-- More efficient than 'fetchAll' since it does not decode any row data.
--
-- @
-- exists <- fetchExists conn userExistsById 42
-- @
fetchExists :: Connection -> Statement p r -> p -> IO Bool
fetchExists conn stmt params = do
  rows <- fetchRowsRaw conn stmt params
  pure (not (null rows))

-- | Like 'fetchAll' but applies a transformation to each decoded row.
-- Useful for mapping database rows to domain types without an intermediate list.
--
-- @
-- names <- fetchAllWith conn listUsers () userName
-- @
fetchAllWith :: Connection -> Statement p r -> p -> (r -> a) -> IO [a]
fetchAllWith conn stmt params f = do
  rows <- fetchAll conn stmt params
  pure (map f rows)

------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------

-- | Execute a command (INSERT\/UPDATE\/DELETE) and return the number of
-- rows affected. The statement result type is fixed to @()@ since no
-- rows are decoded.
--
-- @
-- n <- execute conn insertUser ("Alice", Just "alice\@example.com")
-- @
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

-- | Execute a command with a RETURNING clause. Returns the number of rows
-- affected and the decoded result rows.
--
-- Unlike 'execute' (which discards result rows) or 'fetchAll' (which
-- discards the command tag), this function captures both.
--
-- @
-- (n, ids) <- executeReturning conn insertUsers ("Alice", Just "alice\@example.com")
-- @
executeReturning :: Connection -> Statement p r -> p -> IO (Int64, [r])
executeReturning conn stmt params = do
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
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs CollectRowsAndCommand
  case resp of
    RespRowsAndCommand rawRows tag -> do
      when needsParse $ cacheStmt conn (stmtSQL stmt) name
      decoded <- decodeRows (stmtDecode stmt) rawRows
      pure (tagRows tag, decoded)
    _ -> throwHsqlx (ProtocolError "executeReturning: unexpected response type")

-- | Execute a statement once for each parameter set. Returns the total
-- number of rows affected across all executions. This is an alias for
-- 'executeBatch' with a more common name (matching postgresql-simple's API).
--
-- @
-- total <- executeMany conn insertUser [(\"Alice\", Nothing), (\"Bob\", Just \"b\@x.com\")]
-- @
executeMany :: Connection -> Statement p () -> [p] -> IO Int64
executeMany = executeBatch

------------------------------------------------------------------------
-- Raw (unchecked) queries
------------------------------------------------------------------------

-- | Execute a raw SQL query and return all rows as untyped column vectors.
-- This is the escape hatch for dynamic SQL or queries that don't fit the
-- 'Statement' model. No compile-time type checking is performed.
--
-- Parameters are passed as pre-encoded binary 'ByteString' values (or
-- 'Nothing' for NULL). Parameter OIDs tell Postgres how to interpret them;
-- use OID 0 to let Postgres infer the type.
--
-- @
-- rows <- rawFetchAll conn
--   "SELECT id, name FROM users WHERE role = $1"
--   [25]                -- OID 25 = text
--   [Just "admin"]      -- $1 value
-- @
rawFetchAll
  :: Connection
  -> ByteString
  -- ^ SQL query text
  -> [Word32]
  -- ^ Parameter OIDs (use 0 for Postgres to infer)
  -> [Maybe ByteString]
  -- ^ Parameter values (binary-encoded, or Nothing for NULL)
  -> IO [Vector (Maybe ByteString)]
rawFetchAll conn sql oids params = do
  (name, needsParse) <- lookupOrAllocRaw conn sql
  let paramVec = V.fromList params
      msgs =
        (if needsParse
          then [Parse name sql (V.fromList oids)]
          else [])
          ++ [ Bind "" name binaryFmtVec paramVec binaryFmtVec
             , Execute "" 0
             , Sync
             ]
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs CollectRows
  case resp of
    RespRows rows -> do
      when needsParse $ cacheStmt conn sql name
      pure rows
    _ -> throwHsqlx (ProtocolError "rawFetchAll: unexpected response type")

-- | Execute a raw SQL query and return the first row, or 'Nothing' if the
-- query produces no results. Like 'rawFetchAll' but discards all rows after
-- the first.
rawFetchOne
  :: Connection
  -> ByteString
  -- ^ SQL query text
  -> [Word32]
  -- ^ Parameter OIDs (use 0 for Postgres to infer)
  -> [Maybe ByteString]
  -- ^ Parameter values (binary-encoded, or Nothing for NULL)
  -> IO (Maybe (Vector (Maybe ByteString)))
rawFetchOne conn sql oids params = do
  rows <- rawFetchAll conn sql oids params
  case rows of
    [] -> pure Nothing
    (row : _) -> pure (Just row)

-- | Execute a raw SQL command (INSERT\/UPDATE\/DELETE) and return the number
-- of rows affected. The raw counterpart of 'execute', with no compile-time
-- type checking.
rawExecute
  :: Connection
  -> ByteString
  -- ^ SQL command text
  -> [Word32]
  -- ^ Parameter OIDs (use 0 for Postgres to infer)
  -> [Maybe ByteString]
  -- ^ Parameter values (binary-encoded, or Nothing for NULL)
  -> IO Int64
rawExecute conn sql oids params = do
  (name, needsParse) <- lookupOrAllocRaw conn sql
  let paramVec = V.fromList params
      msgs =
        (if needsParse
          then [Parse name sql (V.fromList oids)]
          else [])
          ++ [ Bind "" name binaryFmtVec paramVec V.empty
             , Execute "" 0
             , Sync
             ]
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs CollectCommand
  case resp of
    RespCommand tag -> do
      when needsParse $ cacheStmt conn sql name
      pure (tagRows tag)
    _ -> throwHsqlx (ProtocolError "rawExecute: unexpected response type")

-- | Look up or allocate a statement name for raw SQL (same cache as typed).
lookupOrAllocRaw :: Connection -> ByteString -> IO (ByteString, Bool)
lookupOrAllocRaw conn sql = do
  cache <- readIORef (connStmtCache conn)
  case Map.lookup sql cache of
    Just name -> pure (name, False)
    Nothing -> do
      evictIfNeeded conn cache
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
      pure (name, True)

------------------------------------------------------------------------
-- Pipelined batch execution
------------------------------------------------------------------------

-- | Execute a batch of commands using pipeline mode, returning the total
-- number of rows affected. All Bind+Execute pairs share a single Sync,
-- eliminating per-item round-trip overhead.
--
-- For batches larger than 256 items, automatically switches to streaming
-- mode (exclusive wire access, chunked sends) to bound memory usage.
--
-- @
-- total <- executeBatch conn insertUser [("Alice", Nothing), ("Bob", Just "b\@x.com")]
-- @
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

-- | Fetch zero or one row for each parameter set, pipelined into a single
-- round trip. Returns a list of results parallel to the input list, where
-- each element is 'Nothing' if that particular query returned no rows.
--
-- @
-- results <- fetchBatchOne conn findById [1, 2, 3]
-- -- results :: [Maybe User]
-- @
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

-- | Fetch all rows for each parameter set, pipelined into a single round
-- trip. Returns a list of result lists parallel to the input list.
--
-- @
-- results <- fetchBatchAll conn listPostsByUser [1, 2, 3]
-- -- results :: [[Post]]
-- @
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
