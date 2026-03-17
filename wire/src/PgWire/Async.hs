{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Asynchronous sender/receiver split for PostgreSQL connections.
--
-- Each connection spawns two green threads after startup:
--
-- * __Writer thread__ — drains the send queue, batches messages via
--   vectored I/O ('sendMany'), and enqueues response MVars.
-- * __Reader thread__ — parses backend messages from the socket and
--   fills MVars in FIFO order.
--
-- Application threads submit 'Request's and block on the returned 'MVar'.
-- PostgreSQL processes messages in strict FIFO order, so the i-th response
-- always corresponds to the i-th pending request — no correlation IDs needed.
--
-- This architecture enables automatic pipelining: when N threads query
-- concurrently, their messages are coalesced into a single 'sendMany'
-- syscall by the writer, and responses are demultiplexed by the reader.
module PgWire.Async
  ( -- * Types
    AsyncWireConn (..)
  , Request (..)
  , Response (..)
  , ResponseCollector (..)
    -- * Lifecycle
  , spawnAsyncWireConn
  , shutdownAsyncWireConn
    -- * Submitting requests
  , submitRequest
  , submitExclusive
  ) where

import Control.Concurrent.Async (Async, async, cancel, link2)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend (FrontendMsg (..))
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsgs)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | What application threads submit to the async connection.
data Request
  = -- | Extended query: pre-built messages (Bind+Execute+Sync) with a collector.
    ReqExtendedQuery ![FrontendMsg] !ResponseCollector
  | -- | Simple text query.
    ReqSimpleQuery !ByteString
  | -- | Parse a prepared statement (Parse+Sync).
    ReqPrepare !FrontendMsg
  | -- | Close a prepared statement (Close+Sync).
    ReqClose !FrontendMsg

-- | What comes back to the caller via MVar.
data Response
  = -- | Rows collected from DataRow messages.
    RespRows ![Vector (Maybe ByteString)]
  | -- | Command tag from CommandComplete.
    RespCommand !CommandTag
  | -- | Simple query result: rows + optional tag.
    RespSimple ![[Maybe ByteString]] !(Maybe CommandTag)
  | -- | ParseComplete acknowledged.
    RespParsed
  | -- | CloseComplete acknowledged.
    RespClosed
  | -- | Multiple sub-results (one per Bind+Execute in a batch).
    RespBatchRows ![[Vector (Maybe ByteString)]]
  | -- | Batch command result: total rows affected.
    RespBatchCommand !Int64

-- | Tells the reader thread how to interpret backend messages for a request.
data ResponseCollector
  = -- | Collect DataRow until CommandComplete, then ReadyForQuery.
    CollectRows
  | -- | Expect CommandComplete + ReadyForQuery, extract row count.
    CollectCommand
  | -- | Expect ParseComplete + ReadyForQuery.
    CollectParseComplete
  | -- | Expect CloseComplete + ReadyForQuery.
    CollectCloseComplete
  | -- | Collect N sub-results (Bind+Execute repeated N times), one ReadyForQuery at end.
    CollectBatch !Int
  | -- | Collect N command sub-results, one ReadyForQuery at end.
    CollectBatchCommand !Int
  | -- | Simple query protocol: text rows + optional tag + ReadyForQuery.
    CollectSimple

-- | Enqueued by the writer, dequeued by the reader.
data PendingResponse = PendingResponse
  { prCollector :: !ResponseCollector
  , prMVar :: !(MVar (Either HsqlxError Response))
  }

-- | The async connection state. Owns the writer and reader threads.
data AsyncWireConn = AsyncWireConn
  { awcWire :: !WireConn
  , awcSendQueue :: !(TBQueue (Request, MVar (Either HsqlxError Response)))
  , awcPending :: !(TQueue PendingResponse)
  , awcExclusive :: !(TMVar (MVar (Either HsqlxError Response)))
  -- ^ When set, the writer pauses the pipeline for exclusive access.
  , awcAlive :: !(TVar Bool)
  , awcTxStatus :: !(IORef TxStatus)
  , awcNotifyHandler :: !(IORef (Int32 -> ByteString -> ByteString -> IO ()))
  , awcNoticeHandler :: !(IORef (PgNotice -> IO ()))
  , awcParamStatus :: !(IORef (Map ByteString ByteString))
  , awcWriterThread :: !(Async ())
  , awcReaderThread :: !(Async ())
  }

-- | Shared state between writer/reader threads. Doesn't include the
-- thread handles themselves (avoids circular dependency with StrictData).
data AsyncCore = AsyncCore
  { acWire :: !WireConn
  , acSendQueue :: !(TBQueue (Request, MVar (Either HsqlxError Response)))
  , acPending :: !(TQueue PendingResponse)
  , acExclusive :: !(TMVar (MVar (Either HsqlxError Response)))
  , acAlive :: !(TVar Bool)
  , acTxStatus :: !(IORef TxStatus)
  , acNotifyHandler :: !(IORef (Int32 -> ByteString -> ByteString -> IO ()))
  , acNoticeHandler :: !(IORef (PgNotice -> IO ()))
  , acParamStatus :: !(IORef (Map ByteString ByteString))
  }

------------------------------------------------------------------------
-- Lifecycle
------------------------------------------------------------------------

-- | Spawn writer and reader threads for an established connection.
-- Call this after the serial startup handshake completes.
spawnAsyncWireConn
  :: WireConn
  -> IORef TxStatus
  -> IORef (Map ByteString ByteString)
  -> IO AsyncWireConn
spawnAsyncWireConn wc txRef paramRef = do
  sendQueue <- newTBQueueIO 64
  pending <- newTQueueIO
  exclusive <- newEmptyTMVarIO
  alive <- newTVarIO True
  notifyHandler <- newIORef (\_ _ _ -> pure ())
  noticeHandler <- newIORef (\_ -> pure ())

  -- The writer and reader threads need access to the shared STM/IORef state
  -- but NOT to each other's Async handles. We build a "core" with just the
  -- shared state, spawn threads from that, then assemble the final record.
  let core = AsyncCore
        { acWire = wc
        , acSendQueue = sendQueue
        , acPending = pending
        , acExclusive = exclusive
        , acAlive = alive
        , acTxStatus = txRef
        , acNotifyHandler = notifyHandler
        , acNoticeHandler = noticeHandler
        , acParamStatus = paramRef
        }

  writer <- async (writerThread core)
  reader <- async (readerThread core)

  -- Link threads: if either dies, both die.
  link2 writer reader

  pure AsyncWireConn
    { awcWire = wc
    , awcSendQueue = sendQueue
    , awcPending = pending
    , awcExclusive = exclusive
    , awcAlive = alive
    , awcTxStatus = txRef
    , awcNotifyHandler = notifyHandler
    , awcNoticeHandler = noticeHandler
    , awcParamStatus = paramRef
    , awcWriterThread = writer
    , awcReaderThread = reader
    }

-- | Shut down the async connection. Signals threads to stop and cancels them.
shutdownAsyncWireConn :: AsyncWireConn -> IO ()
shutdownAsyncWireConn awc = do
  atomically $ writeTVar (awcAlive awc) False
  cancel (awcWriterThread awc) `catch` \(_ :: SomeException) -> pure ()
  cancel (awcReaderThread awc) `catch` \(_ :: SomeException) -> pure ()
  drainPendingWithError awc ConnectionDead

------------------------------------------------------------------------
-- Submitting requests
------------------------------------------------------------------------

-- | Submit a request and block until the response arrives.
-- Throws 'ConnectionDead' if the async threads have died.
submitRequest :: AsyncWireConn -> Request -> IO Response
submitRequest awc req = do
  alive <- readTVarIO (awcAlive awc)
  if not alive
    then throwHsqlx ConnectionDead
    else do
      respVar <- newEmptyMVar
      atomically $ writeTBQueue (awcSendQueue awc) (req, respVar)
      result <- takeMVar respVar
      case result of
        Left err -> throwIO err
        Right resp -> pure resp

-- | Submit an exclusive request. The writer thread will:
-- 1. Stop accepting new requests
-- 2. Wait for all pending responses to be collected by the reader
-- 3. Hand control to the caller
-- 4. Resume normal operation when the caller finishes
--
-- Used for COPY, cursors, folds — operations that need direct socket access.
submitExclusive :: AsyncWireConn -> (WireConn -> IORef TxStatus -> IO a) -> IO a
submitExclusive awc action = do
  alive <- readTVarIO (awcAlive awc)
  if not alive
    then throwHsqlx ConnectionDead
    else do
      -- Signal exclusive mode to the writer thread
      respVar <- newEmptyMVar
      atomically $ putTMVar (awcExclusive awc) respVar

      -- Wait for writer to drain pending and hand us control
      result <- takeMVar respVar
      case result of
        Left err -> throwIO err
        Right _ -> do
          -- We now have exclusive access to the WireConn.
          -- Reader is blocked on empty awcPending.
          -- Writer is blocked waiting for us to signal done.
          a <- action (awcWire awc) (awcTxStatus awc)

          -- Signal done: writer is waiting on takeTMVar (awcExclusive).
          -- We put a dummy MVar, writer takes it and discards.
          doneVar <- newEmptyMVar
          atomically $ putTMVar (awcExclusive awc) doneVar
          putMVar doneVar (Right RespParsed)

          pure a

------------------------------------------------------------------------
-- Writer thread
------------------------------------------------------------------------

data WriterAction
  = WriterExclusive !(MVar (Either HsqlxError Response))
  | WriterBatch ![(Request, MVar (Either HsqlxError Response))]

writerThread :: AsyncCore -> IO ()
writerThread ac = go `catch` onDeath
  where
    go :: IO ()
    go = do
      action <- atomically $ tryExclusive `orElse` drainQueue
      case action of
        WriterExclusive respVar -> handleExclusive respVar
        WriterBatch items -> handleBatch items
      go

    tryExclusive :: STM WriterAction
    tryExclusive = do
      respVar <- takeTMVar (acExclusive ac)
      pure (WriterExclusive respVar)

    drainQueue :: STM WriterAction
    drainQueue = do
      first <- readTBQueue (acSendQueue ac)
      rest <- drainTBQueue (acSendQueue ac)
      pure (WriterBatch (first : rest))

    handleBatch :: [(Request, MVar (Either HsqlxError Response))] -> IO ()
    handleBatch items = do
      let (allMsgs, pendings) = unzip (map buildItem items)
      atomically $ mapM_ (writeTQueue (acPending ac)) pendings
      sendFrontendMsgs (acWire ac) (concat allMsgs)

    buildItem :: (Request, MVar (Either HsqlxError Response)) -> ([FrontendMsg], PendingResponse)
    buildItem (req, respVar) = case req of
      ReqExtendedQuery msgs collector ->
        (msgs, PendingResponse collector respVar)
      ReqSimpleQuery sql ->
        ([Query sql], PendingResponse CollectSimple respVar)
      ReqPrepare parseMsg ->
        ([parseMsg, Flush], PendingResponse CollectParseComplete respVar)
      ReqClose closeMsg ->
        ([closeMsg, Sync], PendingResponse CollectCloseComplete respVar)

    handleExclusive :: MVar (Either HsqlxError Response) -> IO ()
    handleExclusive respVar = do
      atomically $ do
        empty <- isEmptyTQueue (acPending ac)
        check empty
      putMVar respVar (Right RespParsed)
      doneVar <- atomically $ takeTMVar (acExclusive ac)
      _ <- takeMVar doneVar
      pure ()

    onDeath :: SomeException -> IO ()
    onDeath _ = do
      atomically $ writeTVar (acAlive ac) False
      drainPendingWithErrorCore ac ConnectionDead

------------------------------------------------------------------------
-- Reader thread
------------------------------------------------------------------------

readerThread :: AsyncCore -> IO ()
readerThread ac = go `catch` onDeath
  where
    go :: IO ()
    go = do
      pr <- atomically $ readTQueue (acPending ac)
      -- Catch query errors so they don't kill the reader thread.
      -- A QueryError is per-request, not connection-fatal.
      result <- try (collectResponse (prCollector pr))
      case result of
        Right resp -> putMVar (prMVar pr) (Right resp)
        Left (err :: HsqlxError) -> do
          -- After ErrorResponse with Sync, Postgres sends ReadyForQuery.
          -- After ErrorResponse with Flush (preparation), it does NOT.
          case prCollector pr of
            CollectParseComplete -> pure ()
            _ -> drainUntilReady
          putMVar (prMVar pr) (Left err)
      go

    -- Drain messages until ReadyForQuery (error recovery).
    drainUntilReady :: IO ()
    drainUntilReady = do
      msg <- recvAndDispatch
      case msg of
        ReadyForQuery status -> writeIORef (acTxStatus ac) status
        _ -> drainUntilReady

    collectResponse :: ResponseCollector -> IO Response
    collectResponse CollectRows = do
      rows <- collectRowsLoop
      waitReadyForQuery
      pure (RespRows rows)
    collectResponse CollectCommand = do
      tag <- collectCommandLoop
      waitReadyForQuery
      pure (RespCommand tag)
    collectResponse CollectParseComplete = do
      waitParseCompleteLoop
      -- No waitReadyForQuery: Flush doesn't trigger ReadyForQuery.
      pure RespParsed
    collectResponse CollectCloseComplete = do
      waitCloseCompleteLoop
      waitReadyForQuery
      pure RespClosed
    collectResponse (CollectBatch n) = do
      results <- collectBatchLoop n
      waitReadyForQuery
      pure (RespBatchRows results)
    collectResponse (CollectBatchCommand n) = do
      total <- collectBatchCommandLoop n 0
      waitReadyForQuery
      pure (RespBatchCommand total)
    collectResponse CollectSimple = do
      (rows, tag) <- collectSimpleLoop [] Nothing
      pure (RespSimple rows tag)

    -- Collectors skip both ParseComplete and BindComplete so that
    -- Parse+Bind+Execute can be coalesced into a single request.
    collectRowsLoop :: IO [Vector (Maybe ByteString)]
    collectRowsLoop = loop id
      where
        loop !acc = do
          msg <- recvAndDispatch
          case msg of
            ParseComplete -> loop acc
            BindComplete -> loop acc
            DataRow vals -> loop (acc . (vals :))
            CommandComplete _ -> pure (acc [])
            EmptyQueryResponse -> pure (acc [])
            ErrorResponse err -> throwIO (QueryError err)
            other -> throwIO (ProtocolError ("Unexpected in rows: " <> BS8.pack (show other)))

    collectCommandLoop :: IO CommandTag
    collectCommandLoop = do
      msg <- recvAndDispatch
      case msg of
        ParseComplete -> collectCommandLoop
        BindComplete -> collectCommandLoop
        CommandComplete tag -> pure tag
        EmptyQueryResponse -> pure (OtherTag "EMPTY")
        ErrorResponse err -> throwIO (QueryError err)
        other -> throwIO (ProtocolError ("Unexpected in command: " <> BS8.pack (show other)))

    waitParseCompleteLoop :: IO ()
    waitParseCompleteLoop = do
      msg <- recvAndDispatch
      case msg of
        ParseComplete -> pure ()
        ErrorResponse err -> throwIO (QueryError err)
        other -> throwIO (ProtocolError ("Expected ParseComplete, got: " <> BS8.pack (show other)))

    waitCloseCompleteLoop :: IO ()
    waitCloseCompleteLoop = do
      msg <- recvAndDispatch
      case msg of
        CloseComplete -> pure ()
        ErrorResponse _ -> pure ()
        other -> throwIO (ProtocolError ("Expected CloseComplete, got: " <> BS8.pack (show other)))

    collectBatchLoop :: Int -> IO [[Vector (Maybe ByteString)]]
    collectBatchLoop 0 = pure []
    collectBatchLoop n = do
      rows <- collectRowsLoop
      rest <- collectBatchLoop (n - 1)
      pure (rows : rest)

    collectBatchCommandLoop :: Int -> Int64 -> IO Int64
    collectBatchCommandLoop 0 !total = pure total
    collectBatchCommandLoop n !total = do
      msg <- recvAndDispatch
      case msg of
        ParseComplete -> collectBatchCommandLoop n total
        BindComplete -> collectBatchCommandLoop n total
        CommandComplete tag -> collectBatchCommandLoop (n - 1) (total + tagRows tag)
        ErrorResponse err -> throwIO (QueryError err)
        other -> throwIO (ProtocolError ("Unexpected in batch cmd: " <> BS8.pack (show other)))

    collectSimpleLoop :: [[Maybe ByteString]] -> Maybe CommandTag -> IO ([[Maybe ByteString]], Maybe CommandTag)
    collectSimpleLoop !rows !tag = do
      msg <- recvAndDispatch
      case msg of
        RowDescription _ -> collectSimpleLoop rows tag
        DataRow vals -> collectSimpleLoop (V.toList vals : rows) tag
        CommandComplete ct -> collectSimpleLoop rows (Just ct)
        EmptyQueryResponse -> collectSimpleLoop rows tag
        ReadyForQuery status -> do
          writeIORef (acTxStatus ac) status
          pure (reverse rows, tag)
        ErrorResponse err -> throwIO (QueryError err)
        other -> throwIO (ProtocolError ("Unexpected in simple query: " <> BS8.pack (show other)))

    waitReadyForQuery :: IO ()
    waitReadyForQuery = do
      msg <- recvAndDispatch
      case msg of
        ReadyForQuery status -> writeIORef (acTxStatus ac) status
        _ -> waitReadyForQuery

    recvAndDispatch :: IO BackendMsg
    recvAndDispatch = do
      msg <- recvBackendMsg (acWire ac)
      case msg of
        NotificationResponse pid channel payload -> do
          handler <- readIORef (acNotifyHandler ac)
          handler pid channel payload `catch` \(_ :: SomeException) -> pure ()
          recvAndDispatch
        NoticeResponse notice -> do
          handler <- readIORef (acNoticeHandler ac)
          handler notice `catch` \(_ :: SomeException) -> pure ()
          recvAndDispatch
        ParameterStatus key value -> do
          modifyIORef' (acParamStatus ac) (Map.insert key value)
          recvAndDispatch
        _ -> pure msg

    onDeath :: SomeException -> IO ()
    onDeath _ = do
      atomically $ writeTVar (acAlive ac) False
      drainPendingWithErrorCore ac ConnectionDead

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

drainTBQueue :: TBQueue a -> STM [a]
drainTBQueue q = loop id
  where
    loop acc = do
      mItem <- tryReadTBQueue q
      case mItem of
        Just item -> loop (acc . (item :))
        Nothing -> pure (acc [])

drainPendingWithError :: AsyncWireConn -> HsqlxError -> IO ()
drainPendingWithError awc err = do
  pendings <- atomically $ flushTQueue (awcPending awc)
  mapM_ (\pr -> tryPutMVar (prMVar pr) (Left err) >> pure ()) pendings
  queued <- atomically $ drainTBQueue (awcSendQueue awc)
  mapM_ (\(_, mv) -> tryPutMVar mv (Left err) >> pure ()) queued
  mExcl <- atomically $ tryTakeTMVar (awcExclusive awc)
  case mExcl of
    Just mv -> tryPutMVar mv (Left err) >> pure ()
    Nothing -> pure ()

-- | Like 'drainPendingWithError' but operates on 'AsyncCore' (used by threads).
drainPendingWithErrorCore :: AsyncCore -> HsqlxError -> IO ()
drainPendingWithErrorCore ac err = do
  pendings <- atomically $ flushTQueue (acPending ac)
  mapM_ (\pr -> tryPutMVar (prMVar pr) (Left err) >> pure ()) pendings
  queued <- atomically $ drainTBQueue (acSendQueue ac)
  mapM_ (\(_, mv) -> tryPutMVar mv (Left err) >> pure ()) queued
  mExcl <- atomically $ tryTakeTMVar (acExclusive ac)
  case mExcl of
    Just mv -> tryPutMVar mv (Left err) >> pure ()
    Nothing -> pure ()

tagRows :: CommandTag -> Int64
tagRows (InsertTag n) = n
tagRows (UpdateTag n) = n
tagRows (DeleteTag n) = n
tagRows (SelectTag n) = n
tagRows (OtherTag _) = 0
