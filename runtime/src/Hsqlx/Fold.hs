{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Constant-memory row processing via strict folds.
--
-- Process large result sets without buffering all rows in memory
-- and without requiring a transaction or cursor.
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
  stmtName <- ensurePreparedFold conn stmt
  let encodedParams = stmtEncode stmt params
  sendFrontendMsgs (connWire conn)
    [ Bind "" stmtName (V.singleton BinaryFormat) encodedParams (V.singleton BinaryFormat)
    , Execute "" 0
    , Sync
    ]
  collectFold conn (stmtDecode stmt) z0 step

collectFold :: Connection -> (Vector (Maybe ByteString) -> Either String r) -> b -> (b -> r -> b) -> IO b
collectFold conn decode = go
  where
    go !acc step' = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc step'
        DataRow vals -> case decode vals of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right !val -> go (step' acc val) step'
        CommandComplete _ -> go acc step'
        EmptyQueryResponse -> go acc step'
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure acc
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc step'
        other -> throwHsqlx (ProtocolError ("Unexpected in fold: " <> BS8.pack (show other)))

ensurePreparedFold :: Connection -> Statement p r -> IO ByteString
ensurePreparedFold conn stmt = do
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
      waitParseFold conn
      modifyIORef' (connStmtCache conn) (Map.insert sql name)
      pure name

waitParseFold :: Connection -> IO ()
waitParseFold conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ParseComplete -> waitReadyFold conn
    ErrorResponse err -> do
      waitReadyFold conn
      throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected ParseComplete: " <> BS8.pack (show other)))

waitReadyFold :: Connection -> IO ()
waitReadyFold conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ReadyForQuery status -> writeIORef (connTxStatus conn) status
    _ -> waitReadyFold conn
