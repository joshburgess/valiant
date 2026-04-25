{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Streaming adapter for valiant using the @streaming@ library.
--
-- Produces @Stream (Of r) IO ()@ values from query results, enabling
-- composable stream processing.
--
-- @
-- import Valiant
-- import Valiant.Stream
-- import Streaming.Prelude qualified as S
--
-- withTransaction pool $ \\tx -> do
--   count <- S.length_ $
--     selectStream (txConn tx) listAllUsers () 500
--   print count
-- @
module Valiant.Stream
  ( -- * Cursor-based streaming (requires transaction)
    selectStream
    -- * Fold-based streaming (no transaction required)
  , foldStream
  ) where

import Control.Monad.IO.Class (liftIO)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Valiant (Connection, Statement, fetchAll, fetchAllCursor)

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches and
-- yields decoded rows one at a time.
--
-- Note: rows are accumulated in memory before yielding (see
-- 'Valiant.Conduit.selectSource' for the same caveat).
selectStream
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size
  -> Stream (Of r) IO ()
selectStream conn stmt params batchSize = do
  rows <- liftIO (fetchAllCursor conn stmt params batchSize)
  S.each rows

-- | Stream query results using single-shot extended-protocol execution.
--
-- Does not require a transaction.
foldStream
  :: Connection
  -> Statement p r
  -> p
  -> Stream (Of r) IO ()
foldStream conn stmt params = do
  rows <- liftIO (fetchAll conn stmt params)
  S.each rows
