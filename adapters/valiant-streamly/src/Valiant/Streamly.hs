{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Streamly streaming adapter for valiant.
--
-- Produces @Stream IO r@ values from query results, enabling
-- high-performance composable stream processing with streamly.
--
-- @
-- import Valiant
-- import Valiant.Streamly
-- import Streamly.Data.Stream qualified as Stream
-- import Streamly.Data.Fold qualified as Fold
--
-- withTransaction pool $ \\tx -> do
--   count <- Stream.fold Fold.length $
--     selectStreamly (txConn tx) listAllUsers () 500
--   print count
-- @
module Valiant.Streamly
  ( -- * Cursor-based streaming (requires transaction)
    selectStreamly
    -- * Fold-based streaming (no transaction required)
  , foldStreamly
  ) where

import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Valiant (Connection, Statement, fetchAll, fetchAllCursor)

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches and
-- yields decoded rows one at a time.
--
-- Note: rows are accumulated in memory before yielding (see
-- 'Valiant.Conduit.selectSource' for the same caveat).
selectStreamly
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size
  -> Stream IO r
selectStreamly conn stmt params batchSize =
  Stream.concatEffect (Stream.fromList <$> fetchAllCursor conn stmt params batchSize)

-- | Stream query results using single-shot extended-protocol execution.
--
-- Does not require a transaction.
foldStreamly
  :: Connection
  -> Statement p r
  -> p
  -> Stream IO r
foldStreamly conn stmt params =
  Stream.concatEffect (Stream.fromList <$> fetchAll conn stmt params)
