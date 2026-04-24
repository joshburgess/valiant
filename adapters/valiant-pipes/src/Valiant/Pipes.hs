{-# OPTIONS_GHC -fno-full-laziness #-}

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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Pipes (Producer, yield, liftIO)
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Error (ValiantError (..), throwValiant)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid qualified as Oid
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import Valiant.Execute (ensurePrepared)
import Valiant.Statement (Statement (..))
import System.IO.Unsafe (unsafePerformIO)

binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches of the
-- given size, yielding decoded rows one at a time.
selectPipe
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -> Producer r IO ()
selectPipe conn stmt params batchSize = do
  rows <- liftIO $ submitExclusive (connAsync conn) $ \wc txRef -> do
    cursorName <- freshCursorName
    let declareSql = "DECLARE " <> cursorName <> " NO SCROLL CURSOR FOR " <> stmtSQL stmt
        encodedParams = stmtEncode stmt params
        paramOids = V.map Oid.unOid (stmtParamOids stmt)
    sendFrontendMsgs wc
      [ Parse "" declareSql paramOids
      , Bind "" "" binaryFmtVec encodedParams V.empty
      , Execute "" 0
      , Sync
      ]
    waitDeclareComplete wc txRef
    allRows <- fetchAllCursor wc txRef cursorName batchSize (stmtDecode stmt)
    sendFrontendMsg wc (Query ("CLOSE " <> cursorName))
    collectSimpleDiscard wc txRef
    pure allRows
  mapM_ yield rows

-- | Stream query results using row-at-a-time processing.
--
-- Does not require a transaction.
foldPipe
  :: Connection
  -> Statement p r
  -> p
  -> Producer r IO ()
foldPipe conn stmt params = do
  rows <- liftIO $ do
    stmtName <- ensurePrepared conn stmt
    let encodedParams = stmtEncode stmt params
    submitExclusive (connAsync conn) $ \wc txRef -> do
      sendFrontendMsgs wc
        [ Bind "" stmtName binaryFmtVec encodedParams binaryFmtVec
        , Execute "" 0
        , Sync
        ]
      collectAndDecode wc txRef (stmtDecode stmt)
  mapM_ yield rows

------------------------------------------------------------------------
-- Internal (same as other streaming adapters)
------------------------------------------------------------------------

fetchAllCursor
  :: WireConn -> IORef TxStatus -> ByteString -> Int
  -> (Vector (Maybe ByteString) -> Either String r)
  -> IO [r]
fetchAllCursor wc txRef cursorName batchSize decode = go []
  where
    go !acc = do
      let fetchSql = "FETCH FORWARD " <> BS8.pack (show batchSize) <> " FROM " <> cursorName
      sendFrontendMsg wc (Query fetchSql)
      rawRows <- collectFetchResults wc txRef
      if null rawRows
        then pure (reverse acc)
        else do
          decoded <- mapM (\row -> case decode row of
            Left err -> throwValiant (DecodeError (BS8.pack err))
            Right !val -> pure val) rawRows
          go (reverse decoded ++ acc)

collectAndDecode
  :: WireConn -> IORef TxStatus
  -> (Vector (Maybe ByteString) -> Either String r)
  -> IO [r]
collectAndDecode wc txRef decode = go []
  where
    go !acc = do
      msg <- recvBackendMsg wc
      case msg of
        BindComplete -> go acc
        DataRow vals -> case decode vals of
          Left err -> throwValiant (DecodeError (BS8.pack err))
          Right !val -> go (val : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (reverse acc)
        ErrorResponse err -> throwValiant (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwValiant (ProtocolError ("Unexpected in pipe: " <> BS8.pack (show other)))

waitDeclareComplete :: WireConn -> IORef TxStatus -> IO ()
waitDeclareComplete wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ParseComplete -> go
        BindComplete -> go
        CommandComplete _ -> go
        ReadyForQuery status -> writeIORef txRef status
        ErrorResponse err -> throwValiant (QueryError err)
        NoticeResponse _ -> go
        other -> throwValiant (ProtocolError ("Unexpected in DECLARE: " <> BS8.pack (show other)))

collectFetchResults :: WireConn -> IORef TxStatus -> IO [Vector (Maybe ByteString)]
collectFetchResults wc txRef = go []
  where
    go !acc = do
      msg <- recvBackendMsg wc
      case msg of
        RowDescription _ -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (reverse acc)
        ErrorResponse err -> throwValiant (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwValiant (ProtocolError ("Unexpected in cursor fetch: " <> BS8.pack (show other)))

collectSimpleDiscard :: WireConn -> IORef TxStatus -> IO ()
collectSimpleDiscard wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ReadyForQuery status -> writeIORef txRef status
        ErrorResponse err -> throwValiant (QueryError err)
        _ -> go

{-# NOINLINE cursorCounter #-}
cursorCounter :: IORef Word
cursorCounter = unsafePerformIO (newIORef 0)

freshCursorName :: IO ByteString
freshCursorName = do
  n <- atomicModifyIORef' cursorCounter (\n -> (n + 1, n))
  pure ("valiant_pipe_" <> BS8.pack (show n))
