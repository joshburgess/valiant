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
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Async (Request (..), Response (..), ResponseCollector (..), submitRequest)
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Frontend
import Hsqlx.Execute (ensurePrepared)
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

-- | Pipeline a command (INSERT\/UPDATE\/DELETE).
-- Always returns 0 because individual command tags are not tracked in the
-- pipeline collector. Use 'Hsqlx.Execute.execute' outside a pipeline if you
-- need the actual rows-affected count.
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
  let queries = collectQueries pipeline
  case queries of
    [] -> evalPipeline pipeline []
    _ -> do
      -- Ensure all statements are prepared
      names <- mapM (ensurePreparedPipe conn) queries

      -- Build all Bind+Execute messages with one Sync
      let msgs = concatMap (\(PipeQuery stmt p, name) ->
            let encodedParams = stmtEncode stmt p
             in [ Bind "" name (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
                , Execute "" 0
                ]) (zip queries names)
            ++ [Sync]

      -- Submit as a single batch request
      resp <- submitRequest (connAsync conn) $ ReqExtendedQuery msgs (CollectBatch (length queries))

      case resp of
        RespBatchRows results -> evalPipeline pipeline results
        _ -> throwHsqlx (ProtocolError "runPipeline: unexpected response type")

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

ensurePreparedPipe :: Connection -> PipeQuery -> IO ByteString
ensurePreparedPipe conn (PipeQuery stmt _) = ensurePrepared conn stmt
