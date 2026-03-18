-- | PostgreSQL advisory lock helpers.
--
-- Advisory locks are application-level locks managed by PostgreSQL.
-- They are useful for distributed coordination, ensuring only one
-- process runs a particular operation at a time.
--
-- Transaction-scoped locks ('withAdvisoryLockTx') are released
-- automatically when the transaction ends. Session-scoped locks
-- ('withAdvisoryLock') must be explicitly released.
--
-- @
-- -- Transaction-scoped: lock released on COMMIT/ROLLBACK
-- withTransaction pool $ \\tx ->
--   withAdvisoryLockTx (txConn tx) 42 $
--     execute (txConn tx) exclusiveStmt params
--
-- -- Session-scoped with bracket
-- withResource pool $ \\conn ->
--   withAdvisoryLock conn 42 $
--     doExclusiveWork conn
-- @
module Hsqlx.Advisory
  ( -- * Session-scoped locks
    withAdvisoryLock
  , withAdvisoryLockTry
  , advisoryLock
  , advisoryUnlock
    -- * Transaction-scoped locks
  , withAdvisoryLockTx
  , withAdvisoryLockTxTry
  , advisoryLockTx
  ) where

import Control.Exception (bracket_)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int64)
import PgWire.Connection (Connection, simpleQuery)

-- | Acquire a session-scoped advisory lock, run an action, then release.
-- Blocks until the lock is available.
--
-- @
-- withAdvisoryLock conn 12345 $ do
--   -- exclusive access guaranteed
--   ...
-- @
withAdvisoryLock :: Connection -> Int64 -> IO a -> IO a
withAdvisoryLock conn key =
  bracket_ (advisoryLock conn key) (advisoryUnlock conn key)

-- | Try to acquire a session-scoped advisory lock without blocking.
-- Returns 'Nothing' if the lock is already held.
--
-- @
-- result <- withAdvisoryLockTry conn 12345 doWork
-- case result of
--   Just val -> putStrLn \"Got the lock\"
--   Nothing  -> putStrLn \"Lock is held by another session\"
-- @
withAdvisoryLockTry :: Connection -> Int64 -> IO a -> IO (Maybe a)
withAdvisoryLockTry conn key action = do
  acquired <- advisoryTryLock conn key
  if acquired
    then do
      result <- action
      advisoryUnlock conn key
      pure (Just result)
    else pure Nothing

-- | Acquire a session-scoped advisory lock. Blocks until available.
advisoryLock :: Connection -> Int64 -> IO ()
advisoryLock conn key = do
  _ <- simpleQuery conn ("SELECT pg_advisory_lock(" <> BS8.pack (show key) <> ")")
  pure ()

-- | Release a session-scoped advisory lock.
advisoryUnlock :: Connection -> Int64 -> IO ()
advisoryUnlock conn key = do
  _ <- simpleQuery conn ("SELECT pg_advisory_unlock(" <> BS8.pack (show key) <> ")")
  pure ()

-- | Try to acquire a session-scoped advisory lock without blocking.
-- Returns 'True' if acquired.
advisoryTryLock :: Connection -> Int64 -> IO Bool
advisoryTryLock conn key = do
  (rows, _) <- simpleQuery conn ("SELECT pg_try_advisory_lock(" <> BS8.pack (show key) <> ")")
  pure $ case rows of
    [[Just "t"]] -> True
    _ -> False

-- | Acquire a transaction-scoped advisory lock, run an action.
-- The lock is automatically released when the transaction ends.
-- Must be called within a transaction.
--
-- @
-- withTransaction pool $ \\tx ->
--   withAdvisoryLockTx (txConn tx) 42 $
--     execute (txConn tx) stmt params
-- @
withAdvisoryLockTx :: Connection -> Int64 -> IO a -> IO a
withAdvisoryLockTx conn key action = do
  advisoryLockTx conn key
  action

-- | Try to acquire a transaction-scoped advisory lock without blocking.
-- Returns 'Nothing' if the lock is already held.
withAdvisoryLockTxTry :: Connection -> Int64 -> IO a -> IO (Maybe a)
withAdvisoryLockTxTry conn key action = do
  acquired <- advisoryTryLockTx conn key
  if acquired
    then Just <$> action
    else pure Nothing

-- | Acquire a transaction-scoped advisory lock. Blocks until available.
-- Released automatically on COMMIT/ROLLBACK.
advisoryLockTx :: Connection -> Int64 -> IO ()
advisoryLockTx conn key = do
  _ <- simpleQuery conn ("SELECT pg_advisory_xact_lock(" <> BS8.pack (show key) <> ")")
  pure ()

-- | Try to acquire a transaction-scoped advisory lock without blocking.
advisoryTryLockTx :: Connection -> Int64 -> IO Bool
advisoryTryLockTx conn key = do
  (rows, _) <- simpleQuery conn ("SELECT pg_try_advisory_xact_lock(" <> BS8.pack (show key) <> ")")
  pure $ case rows of
    [[Just "t"]] -> True
    _ -> False
