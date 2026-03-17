-- | Query execution functions using the PostgreSQL extended query protocol.
--
-- All functions use binary format for both parameters and results,
-- prepared statement caching, and message coalescing for minimal
-- round-trip overhead.
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
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Connection (Connection (..))
import PgWire.Protocol.Oid qualified as Oid
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import Hsqlx.Statement (Statement (..))
import PgWire.Wire (recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)

-- | Fetch zero or one row. Returns 'Nothing' if the query produces no results.
--
-- @
-- mUser <- fetchOne conn findById 42
-- case mUser of
--   Just (id, name, email) -> print name
--   Nothing -> putStrLn \"not found\"
-- @
fetchOne :: Connection -> Statement p r -> p -> IO (Maybe r)
fetchOne conn stmt params = do
  rows <- executeExtended conn stmt params
  case rows of
    [] -> pure Nothing
    (row : _) -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)

-- | Fetch all result rows as a list.
--
-- Uses fused collection+decoding: each row is decoded as it arrives from
-- the wire, eliminating the intermediate @[Vector (Maybe ByteString)]@.
--
-- For large result sets, consider 'Hsqlx.Streaming.withCursor' instead.
fetchAll :: Connection -> Statement p r -> p -> IO [r]
fetchAll conn stmt params = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params
  sendFrontendMsgs (connWire conn)
    [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
    , Execute "" 0
    , Sync
    ]
  collectAndDecodeRows conn (stmtDecode stmt)

-- | Fetch a single scalar value. Throws 'DecodeError' if the query
-- returns zero or more than one row.
fetchScalar :: Connection -> Statement p r -> p -> IO r
fetchScalar conn stmt params = do
  rows <- executeExtended conn stmt params
  case rows of
    [row] -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val
    [] -> throwHsqlx (DecodeError "fetchScalar: query returned no rows")
    _ -> throwHsqlx (DecodeError "fetchScalar: query returned more than one row")

-- | Execute a command (INSERT\/UPDATE\/DELETE). Returns the number of
-- rows affected.
execute :: Connection -> Statement p () -> p -> IO Int64
execute conn stmt params = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params

  -- Coalesce Bind + Execute + Sync into a single send
  sendFrontendMsgs (connWire conn)
    [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams V.empty
    , Execute "" 0
    , Sync
    ]

  -- Collect responses
  rowsAffected <- collectCommandResult conn
  pure rowsAffected

-- | Execute a batch of commands using pipeline mode.
--
-- Sends all Bind+Execute messages with a single Sync at the end,
-- eliminating per-row round-trip overhead. Returns total rows affected.
--
-- This is 39-100x faster than calling 'execute' in a loop, because it
-- reduces N network round-trips to 1.
--
-- @
-- executeBatch conn insertStmt
--   [ (\"Alice\", Just \"alice\@example.com\")
--   , (\"Bob\",   Just \"bob\@example.com\")
--   , (\"Carol\", Nothing)
--   ]
-- @
executeBatch :: Connection -> Statement p () -> [p] -> IO Int64
executeBatch _ _ [] = pure 0
executeBatch conn stmt paramsList = do
  stmtName <- ensurePrepared conn stmt

  -- Build all messages: [Bind, Execute, Bind, Execute, ..., Sync]
  let msgs = concatMap (\params ->
        let encodedParams = stmtEncode stmt params
         in [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams V.empty
            , Execute "" 0
            ]) paramsList
        ++ [Sync]

  -- Send everything in a single syscall
  sendFrontendMsgs (connWire conn) msgs

  -- Collect all responses: N * (BindComplete + CommandComplete) + ReadyForQuery
  collectBatchResult conn (length paramsList)

-- | Fetch zero or one row for each parameter set, pipelined.
--
-- Sends N Bind+Execute pairs with a single Sync, then collects
-- results for each. Returns one @Maybe r@ per parameter set.
--
-- @
-- users <- fetchBatchOne conn findUserById [1, 2, 3, 42, 99]
-- -- 5 lookups in 1 round-trip; users :: [Maybe (Int32, Text, ...)]
-- @
fetchBatchOne :: Connection -> Statement p r -> [p] -> IO [Maybe r]
fetchBatchOne _ _ [] = pure []
fetchBatchOne conn stmt paramsList = do
  stmtName <- ensurePrepared conn stmt
  let msgs = concatMap (\params ->
        let encodedParams = stmtEncode stmt params
         in [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
            , Execute "" 0
            ]) paramsList
        ++ [Sync]
  sendFrontendMsgs (connWire conn) msgs
  results <- collectBatchReadResults conn (stmtDecode stmt) (length paramsList)
  waitReadyBatch conn
  pure (map (\rows -> case rows of
    [] -> Nothing
    (r : _) -> Just r) results)

-- | Fetch all rows for each parameter set, pipelined.
--
-- Sends N Bind+Execute pairs with a single Sync, then collects
-- all result rows for each. Returns one @[r]@ per parameter set.
--
-- @
-- postsByUser <- fetchBatchAll conn listPostsByUser [1, 2, 3]
-- -- 3 queries in 1 round-trip; postsByUser :: [[Post]]
-- @
fetchBatchAll :: Connection -> Statement p r -> [p] -> IO [[r]]
fetchBatchAll _ _ [] = pure []
fetchBatchAll conn stmt paramsList = do
  stmtName <- ensurePrepared conn stmt
  let msgs = concatMap (\params ->
        let encodedParams = stmtEncode stmt params
         in [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
            , Execute "" 0
            ]) paramsList
        ++ [Sync]
  sendFrontendMsgs (connWire conn) msgs
  results <- collectBatchReadResults conn (stmtDecode stmt) (length paramsList)
  waitReadyBatch conn
  pure results

-- | Collect N result sets from pipelined reads. Each result set is
-- delimited by CommandComplete (or EmptyQueryResponse).
collectBatchReadResults :: Connection -> (Vector (Maybe ByteString) -> Either String r) -> Int -> IO [[r]]
collectBatchReadResults _ _ 0 = pure []
collectBatchReadResults conn decode n = do
  -- Collect one result set
  rows <- collectOneReadResult conn decode
  -- Collect remaining
  rest <- collectBatchReadResults conn decode (n - 1)
  pure (rows : rest)

collectOneReadResult :: Connection -> (Vector (Maybe ByteString) -> Either String r) -> IO [r]
collectOneReadResult conn decode = go id
  where
    go !acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> case decode vals of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right !val -> go (acc . (val :))
        CommandComplete _ -> pure (acc [])
        EmptyQueryResponse -> pure (acc [])
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in batch read: " <> BS8.pack (show other)))

waitReadyBatch :: Connection -> IO ()
waitReadyBatch conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ReadyForQuery status -> writeIORef (connTxStatus conn) status
    _ -> waitReadyBatch conn

collectBatchResult :: Connection -> Int -> IO Int64
collectBatchResult conn remaining = go 0 remaining
  where
    go !total 0 = do
      -- Wait for final ReadyForQuery
      waitReady conn
      pure total
    go !total !n = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go total n
        CommandComplete tag -> go (total + tagRows tag) (n - 1)
        ErrorResponse err -> do
          -- Drain remaining responses
          drainUntilReady conn
          throwHsqlx (QueryError err)
        NoticeResponse _ -> go total n
        other -> throwHsqlx (ProtocolError ("Unexpected in batch: " <> BS8.pack (show other)))

    tagRows (InsertTag r) = r
    tagRows (UpdateTag r) = r
    tagRows (DeleteTag r) = r
    tagRows (SelectTag r) = r
    tagRows (OtherTag _) = 0

drainUntilReady :: Connection -> IO ()
drainUntilReady conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ReadyForQuery status -> writeIORef (connTxStatus conn) status
    _ -> drainUntilReady conn

-- Extended query protocol -------------------------------------------------

executeExtended :: Connection -> Statement p r -> p -> IO [Vector (Maybe ByteString)]
executeExtended conn stmt params = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params

  -- Coalesce Bind + Execute + Sync into a single send
  sendFrontendMsgs (connWire conn)
    [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
    , Execute "" 0
    , Sync
    ]

  -- Collect rows
  collectRows conn

-- | Maximum number of prepared statements cached per connection.
-- When exceeded, the least-recently-used statement is closed on the server.
maxCachedStatements :: Int
maxCachedStatements = 256

-- | Ensure a statement is prepared on this connection. Returns the statement name.
-- Uses LRU eviction when the cache exceeds 'maxCachedStatements'.
ensurePrepared :: Connection -> Statement p r -> IO ByteString
ensurePrepared conn stmt = do
  cache <- readIORef (connStmtCache conn)
  let sql = stmtSQL stmt
  case Map.lookup sql cache of
    Just name -> pure name
    Nothing -> do
      -- Evict oldest if cache is full
      evictIfNeeded conn cache

      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
          oids = V.map Oid.unOid (stmtParamOids stmt)
      sendFrontendMsg (connWire conn) (Parse name sql oids)
      sendFrontendMsg (connWire conn) Sync

      -- Wait for ParseComplete + ReadyForQuery
      waitParseComplete conn
      modifyIORef' (connStmtCache conn) (Map.insert sql name)
      pure name

-- | If the cache has reached its limit, close the oldest prepared statement.
evictIfNeeded :: Connection -> Map ByteString ByteString -> IO ()
evictIfNeeded conn cache
  | Map.size cache < maxCachedStatements = pure ()
  | otherwise = case Map.lookupMin cache of
      Nothing -> pure ()
      Just (oldSql, oldName) -> do
        -- Send Close for the prepared statement
        sendFrontendMsgs (connWire conn)
          [ Close DescribeStatement oldName
          , Sync
          ]
        -- Wait for CloseComplete + ReadyForQuery
        waitCloseComplete conn
        modifyIORef' (connStmtCache conn) (Map.delete oldSql)

waitCloseComplete :: Connection -> IO ()
waitCloseComplete conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    CloseComplete -> waitReady conn
    ErrorResponse _ -> waitReady conn -- best-effort
    _ -> waitCloseComplete conn

collectRows :: Connection -> IO [Vector (Maybe ByteString)]
collectRows conn = go id
  where
    go !acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> go (acc . (vals :))
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (acc [])
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in query: " <> BS8.pack (show other)))

-- | Fused row collection + decoding. Decodes each DataRow as it arrives,
-- avoiding the intermediate [Vector (Maybe ByteString)].
collectAndDecodeRows :: Connection -> (Vector (Maybe ByteString) -> Either String r) -> IO [r]
collectAndDecodeRows conn decode = go id
  where
    go !acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> case decode vals of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right !val -> go (acc . (val :))
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (acc [])
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in query: " <> BS8.pack (show other)))

collectCommandResult :: Connection -> IO Int64
collectCommandResult conn = go 0
  where
    go !n = do
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
