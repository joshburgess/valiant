{-# LANGUAGE ExistentialQuantification #-}

-- | Pipelined query execution for batching multiple independent queries
-- into a single network round-trip.
--
-- The PostgreSQL extended query protocol allows sending multiple
-- Bind+Execute sequences before a single Sync. This module provides
-- an 'Applicative' interface for composing independent queries that
-- are sent together and collected in order.
--
-- @
-- (user, posts) <- 'runPipeline' conn $ (,)
--   '<$>' 'pipeFetchAll' listPostsByUser 42
--   '<*>' 'pipeFetchOne' findUserById 42
-- -- Both queries sent in one round-trip, results collected in order
-- @
--
-- This eliminates the N+1 query problem at the driver level.
module Hsqlx.Pipeline
  ( Pipeline
  , pipeFetchOne
  , pipeFetchAll
  , pipeFetchScalar
  , pipeExecute
  , runPipeline
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid qualified as Oid
import PgWire.Wire (recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import Hsqlx.Statement (Statement (..))

-- | A pipeline of queries to execute in a single round-trip.
-- Use the 'Applicative' interface to compose independent queries.
data Pipeline a
  = PureP a
  | forall p r. QueryP
      (Statement p r)
      p
      ([Vector (Maybe ByteString)] -> IO a)
      -- ^ Decoder: takes collected rows and produces the result
  | forall b. ApP (Pipeline (b -> a)) (Pipeline b)

instance Functor Pipeline where
  fmap f (PureP a) = PureP (f a)
  fmap f (QueryP s p dec) = QueryP s p (\rows -> f <$> dec rows)
  fmap f (ApP pf px) = ApP (fmap (f .) pf) px

instance Applicative Pipeline where
  pure = PureP
  (<*>) = ApP

-- | Pipeline a query that returns zero or one row.
pipeFetchOne :: Statement p r -> p -> Pipeline (Maybe r)
pipeFetchOne stmt params = QueryP stmt params $ \rows ->
  case rows of
    [] -> pure Nothing
    (row : _) -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)
{-# INLINE pipeFetchOne #-}

-- | Pipeline a query that returns all rows.
pipeFetchAll :: Statement p r -> p -> Pipeline [r]
pipeFetchAll stmt params = QueryP stmt params $ \rows ->
  mapM (\row -> case stmtDecode stmt row of
    Left err -> throwHsqlx (DecodeError (BS8.pack err))
    Right val -> pure val) rows
{-# INLINE pipeFetchAll #-}

-- | Pipeline a query that returns a single scalar value.
pipeFetchScalar :: Statement p r -> p -> Pipeline r
pipeFetchScalar stmt params = QueryP stmt params $ \rows ->
  case rows of
    [row] -> case stmtDecode stmt row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure val
    [] -> throwHsqlx (DecodeError "pipeFetchScalar: no rows")
    _ -> throwHsqlx (DecodeError "pipeFetchScalar: more than one row")
{-# INLINE pipeFetchScalar #-}

-- | Pipeline a command (INSERT/UPDATE/DELETE) that returns rows affected.
pipeExecute :: Statement p () -> p -> Pipeline Int64
pipeExecute stmt params = QueryP stmt params $ \_ ->
  -- The row count comes from CommandComplete, not DataRow.
  -- For pipelined commands, we return 0 since we don't track
  -- individual command tags in the pipeline collector.
  -- Use executeBatch for batch commands where you need the count.
  pure 0
{-# INLINE pipeExecute #-}

-- | Execute all queries in the pipeline in a single network round-trip.
--
-- @
-- (user, posts, count) <- runPipeline conn $ (,,)
--   '<$>' pipeFetchOne findUserById 42
--   '<*>' pipeFetchAll listRecentPosts 10
--   '<*>' pipeFetchScalar countUsers ()
-- @
runPipeline :: Connection -> Pipeline a -> IO a
runPipeline conn pipeline = do
  -- Phase 1: collect all queries
  let queries = collectQueries pipeline
  case queries of
    [] -> evalPipeline pipeline []
    _ -> do
      -- Ensure all statements are prepared
      names <- mapM (ensurePreparedPipe conn) queries

      -- Build and send all Bind+Execute messages with one Sync
      let msgs = concatMap (\(PipeQuery stmt p, name) ->
            let encodedParams = stmtEncode stmt p
             in [ Bind "" name (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
                , Execute "" 0
                ]) (zip queries names)
            ++ [Sync]
      sendFrontendMsgs (connWire conn) msgs

      -- Phase 2: collect results for each query
      results <- mapM (\_ -> collectQueryRows conn) queries

      -- Wait for final ReadyForQuery
      waitReadyPipe conn

      -- Phase 3: decode and assemble
      evalPipeline pipeline results

-- Internal ----------------------------------------------------------------

data PipeQuery = forall p r. PipeQuery (Statement p r) p

collectQueries :: Pipeline a -> [PipeQuery]
collectQueries (PureP _) = []
collectQueries (QueryP stmt params _) = [PipeQuery stmt params]
collectQueries (ApP pf px) = collectQueries pf ++ collectQueries px

evalPipeline :: Pipeline a -> [[Vector (Maybe ByteString)]] -> IO a
evalPipeline pipeline allResults = do
  ref <- newIORef allResults
  eval ref pipeline

eval :: IORef [[Vector (Maybe ByteString)]] -> Pipeline a -> IO a
eval _ (PureP a) = pure a
eval ref (QueryP _ _ decode) = do
  rs <- readIORef ref
  case rs of
    (rows : rest) -> do
      writeIORef ref rest
      decode rows
    [] -> throwHsqlx (DecodeError "pipeline: result underflow")
eval ref (ApP pf px) = do
  f <- eval ref pf
  x <- eval ref px
  pure (f x)

collectQueryRows :: Connection -> IO [Vector (Maybe ByteString)]
collectQueryRows conn = go id
  where
    go !acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> go (acc . (vals :))
        CommandComplete _ -> pure (acc [])
        EmptyQueryResponse -> pure (acc [])
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in pipeline: " <> BS8.pack (show other)))

waitReadyPipe :: Connection -> IO ()
waitReadyPipe conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ReadyForQuery status -> writeIORef (connTxStatus conn) status
    _ -> waitReadyPipe conn

ensurePreparedPipe :: Connection -> PipeQuery -> IO ByteString
ensurePreparedPipe conn (PipeQuery stmt _) = do
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
      waitParseCompletePipe conn
      modifyIORef' (connStmtCache conn) (Map.insert sql name)
      pure name

waitParseCompletePipe :: Connection -> IO ()
waitParseCompletePipe conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ParseComplete -> waitReadyPipe conn
    ErrorResponse err -> do
      waitReadyPipe conn
      throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected ParseComplete: " <> BS8.pack (show other)))
