-- | Conduit streaming adapter for valiant.
--
-- Stream query results as @ConduitT@ sources, enabling integration with
-- the conduit ecosystem for composable stream processing.
--
-- Two streaming strategies are provided:
--
-- * 'selectSource' — cursor-based, requires a transaction, fetches in
--   configurable batches from the server.
-- * 'foldSource' — single-shot extended-protocol query, no transaction
--   required.
--
-- @
-- import Valiant
-- import Valiant.Conduit
-- import Conduit
--
-- withTransaction pool $ \\tx ->
--   runConduit $
--     selectSource (txConn tx) listAllUsers () 500
--     .| mapC userName
--     .| sinkList
-- @
module Valiant.Conduit
  ( -- * Cursor-based streaming (requires transaction)
    selectSource
    -- * Fold-based streaming (no transaction required)
  , foldSource
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (ConduitT, yield)
import Valiant (Connection, Statement, fetchAll, fetchAllCursor)

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches of the
-- given size, yielding decoded rows one at a time into the conduit.
--
-- Note: rows are accumulated in memory before yielding because cursor
-- access requires an exclusive wire session that completes before the
-- conduit can run. For incremental memory use, see 'foldSource' (which
-- returns a single result set) or use the lower-level cursor API
-- ('Valiant.withCursor' / 'Valiant.fetchBatch') directly.
selectSource
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size (number of rows per FETCH)
  -> ConduitT () r IO ()
selectSource conn stmt params batchSize = do
  rows <- liftIO (fetchAllCursor conn stmt params batchSize)
  mapM_ yield rows

-- | Stream query results using single-shot extended-protocol execution.
--
-- Does not require a transaction.
foldSource
  :: Connection
  -> Statement p r
  -> p
  -> ConduitT () r IO ()
foldSource conn stmt params = do
  rows <- liftIO (fetchAll conn stmt params)
  mapM_ yield rows
