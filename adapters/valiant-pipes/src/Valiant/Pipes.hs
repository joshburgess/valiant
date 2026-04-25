-- | Pipes streaming adapter for valiant.
--
-- Produces @Producer r IO ()@ values from query results, enabling
-- composable stream processing with the pipes ecosystem.
--
-- @
-- import Valiant
-- import Valiant.Pipes
-- import Pipes
-- import Pipes.Prelude qualified as P
--
-- withTransaction pool $ \\tx ->
--   runEffect $
--     selectPipe (txConn tx) listAllUsers () 500
--     >-> P.map userName
--     >-> P.stdoutLn
-- @
module Valiant.Pipes
  ( -- * Cursor-based streaming (requires transaction)
    selectPipe
    -- * Fold-based streaming (no transaction required)
  , foldPipe
  ) where

import Pipes (Producer, liftIO, yield)
import Valiant (Connection, Statement, fetchAll, fetchAllCursor)

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches of the
-- given size, yielding decoded rows one at a time.
--
-- Note: rows are accumulated in memory before yielding (see
-- 'Valiant.Conduit.selectSource' for the same caveat).
selectPipe
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -> Producer r IO ()
selectPipe conn stmt params batchSize = do
  rows <- liftIO (fetchAllCursor conn stmt params batchSize)
  mapM_ yield rows

-- | Stream query results using single-shot extended-protocol execution.
--
-- Does not require a transaction.
foldPipe
  :: Connection
  -> Statement p r
  -> p
  -> Producer r IO ()
foldPipe conn stmt params = do
  rows <- liftIO (fetchAll conn stmt params)
  mapM_ yield rows
