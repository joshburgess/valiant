-- | UNNEST/ANY-based batch fetching for loading multiple entities by ID
-- in a single query.
--
-- Instead of N queries or N pipelined queries, this rewrites a
-- single-parameter lookup into a set-returning query using
-- @WHERE id = ANY($1::type[])@. PostgreSQL can optimize this into
-- a single index scan.
--
-- @
-- -- Fetch users 1, 2, 3, 42, 99 in one query:
-- users <- 'fetchByIds' conn
--   \"SELECT id, name, email FROM users WHERE id = ANY($1::int4[])\"
--   [23]   -- element OID (int4)
--   [1, 2, 3, 42, 99]
-- -- users :: [(Int32, Text, Maybe Text)]
-- @
--
-- For the common pattern of loading entities by a list of IDs, this is
-- faster than pipelining because PostgreSQL handles one query plan
-- instead of N.
module Hsqlx.Batch
  ( fetchByIds
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import PgWire.Binary.Types (PgEncode (..))
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid (Oid (..))
import PgWire.Wire (recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import Hsqlx.Binary.Array (pgEncodeArray)
import Hsqlx.FromRow (FromRow (..))
import Data.IORef
import Data.Map.Strict qualified as Map

-- | Fetch rows matching any of the given IDs in a single query.
--
-- The SQL must use @$1@ as an array parameter with @= ANY($1::type[])@.
-- The element OID specifies the array element type (e.g., 23 for int4).
--
-- @
-- users <- fetchByIds conn
--   \"SELECT id, name, email FROM users WHERE id = ANY($1::int4[])\"
--   23   -- int4 element OID
--   [1, 2, 3, 42, 99]
-- @
fetchByIds
  :: (PgEncode a, FromRow r)
  => Connection
  -> ByteString
  -- ^ SQL with @$1@ as the array parameter
  -> Oid
  -- ^ Element type OID (e.g., 'PgWire.Protocol.Oid.oidInt4')
  -> [a]
  -- ^ IDs to fetch
  -> IO [r]
fetchByIds conn sql elemOid ids = do
  let arrayBytes = pgEncodeArray elemOid (V.fromList ids)
      arrayOid = arrayOidFor elemOid

  -- Prepare
  stmtName <- ensurePreparedRaw conn sql (V.singleton (unOid arrayOid))

  -- Bind with the array parameter + Execute + Sync
  sendFrontendMsgs (connWire conn)
    [ Bind "" stmtName
        (V.singleton BinaryFormat)
        (V.singleton (Just arrayBytes))
        (V.singleton BinaryFormat)
    , Execute "" 0
    , Sync
    ]

  -- Collect and decode rows
  collectAndDecode conn

-- Map element OID to array OID
arrayOidFor :: Oid -> Oid
arrayOidFor (Oid 16)   = Oid 1000  -- bool[]
arrayOidFor (Oid 17)   = Oid 1001  -- bytea[]
arrayOidFor (Oid 20)   = Oid 1016  -- int8[]
arrayOidFor (Oid 21)   = Oid 1005  -- int2[]
arrayOidFor (Oid 23)   = Oid 1007  -- int4[]
arrayOidFor (Oid 25)   = Oid 1009  -- text[]
arrayOidFor (Oid 1043) = Oid 1015  -- varchar[]
arrayOidFor (Oid 700)  = Oid 1021  -- float4[]
arrayOidFor (Oid 701)  = Oid 1022  -- float8[]
arrayOidFor (Oid 1114) = Oid 1115  -- timestamp[]
arrayOidFor (Oid 1184) = Oid 1185  -- timestamptz[]
arrayOidFor (Oid 1082) = Oid 1182  -- date[]
arrayOidFor (Oid 2950) = Oid 2951  -- uuid[]
arrayOidFor oid        = oid       -- fallback: use as-is

-- Internal helpers --------------------------------------------------------

ensurePreparedRaw :: Connection -> ByteString -> Vector Word32 -> IO ByteString
ensurePreparedRaw conn sql oids = do
  cache <- readIORef (connStmtCache conn)
  case Map.lookup sql cache of
    Just name -> pure name
    Nothing -> do
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
      sendFrontendMsg (connWire conn) (Parse name sql oids)
      sendFrontendMsg (connWire conn) Sync
      waitParse conn
      modifyIORef' (connStmtCache conn) (Map.insert sql name)
      pure name

collectAndDecode :: (FromRow r) => Connection -> IO [r]
collectAndDecode conn = go id
  where
    go !acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        BindComplete -> go acc
        DataRow vals -> case fromRow vals of
          Left err -> throwHsqlx (DecodeError (BS8.pack err))
          Right !val -> go (acc . (val :))
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (acc [])
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in batch fetch: " <> BS8.pack (show other)))

waitParse :: Connection -> IO ()
waitParse conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    ParseComplete -> waitReady conn
    ErrorResponse err -> do
      waitReady conn
      throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected ParseComplete: " <> BS8.pack (show other)))
  where
    waitReady c = do
      m <- recvBackendMsg (connWire c)
      case m of
        ReadyForQuery status -> writeIORef (connTxStatus c) status
        _ -> waitReady c
