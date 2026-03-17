{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Constant-memory row processing via strict folds.
--
-- Process large result sets without buffering all rows in memory
-- and without requiring a transaction or cursor.
--
-- Folds require exclusive access to the connection's wire so that rows
-- can be processed one-at-a-time directly from the socket.
--
-- @
-- -- Count and sum in one pass, constant memory:
-- (count, total) <- executeWithFold conn stmt params $
--   RowFold (0, 0) (\\(!c, !t) (_, _, score) -> (c + 1, t + score))
-- @
module Hsqlx.Fold
  ( RowFold (..)
  , executeWithFold
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsgs)
import Hsqlx.Execute (ensurePrepared)
import Hsqlx.Statement (Statement (..))

-- | A strict left fold over result rows. Processes rows in constant
-- memory as they arrive from the wire — no list, no buffering.
data RowFold a b = RowFold
  { foldInit :: !b
  -- ^ Initial accumulator value.
  , foldStep :: !(b -> a -> b)
  -- ^ Strict step function. Apply bang patterns to accumulator components.
  }

-- | Execute a statement and fold over the results in constant memory.
--
-- Each row is decoded and fed to the fold as it arrives from the wire.
-- The entire result set is never held in memory.
--
-- @
-- total <- executeWithFold conn countStmt () $
--   RowFold 0 (\\acc (Only n) -> acc + n)
-- @
executeWithFold :: Connection -> Statement p r -> p -> RowFold r b -> IO b
executeWithFold conn stmt params (RowFold z0 step) = do
  stmtName <- ensurePrepared conn stmt
  let encodedParams = stmtEncode stmt params
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsgs wc
      [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
      , Execute "" 0
      , Sync
      ]
    collectFold wc txRef (stmtDecode stmt) z0 step

collectFold :: WireConn -> IORef TxStatus -> (Vector (Maybe ByteString) -> Either String r) -> b -> (b -> r -> b) -> IO b
collectFold wc txRef decode = go
  where
    go !acc step' = do
      msg <- recvBackendMsg wc
      case msg of
        BindComplete -> go acc step'
        DataRow vals -> case decode vals of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right !val -> go (step' acc val) step'
        CommandComplete _ -> go acc step'
        EmptyQueryResponse -> go acc step'
        ReadyForQuery status -> do
          writeIORef txRef status
          pure acc
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc step'
        other -> throwHsqlx (ProtocolError ("Unexpected in fold: " <> BS8.pack (show other)))
