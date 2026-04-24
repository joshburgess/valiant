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
module Valiant.Batch
  ( fetchByIds
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.HashPSQ qualified as PSQ
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import PgWire.Async (Request (..), Response (..), ResponseCollector (..), submitRequest)
import PgWire.Binary.Types (PgEncode (..))
import PgWire.Connection (Connection (..))
import PgWire.Error (ValiantError (..), throwValiant)
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid (Oid (..))
import Valiant.Binary.Array (pgEncodeArray)
import Valiant.FromRow (FromRow (..))

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

  -- Bind + Execute + Sync via async channel
  resp <- submitRequest (connAsync conn) $ ReqExtendedQuery
    [ Bind "" stmtName
        (V.singleton BinaryFormat)
        (V.singleton (Just arrayBytes))
        (V.singleton BinaryFormat)
    , Execute "" 0
    , Sync
    ]
    CollectRows

  case resp of
    RespRows rows -> mapM (\row -> case fromRow row of
      Left err -> throwValiant (DecodeError (BS8.pack err))
      Right !val -> pure val) rows
    _ -> throwValiant (ProtocolError "fetchByIds: unexpected response type")

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

-- | Bump the monotonic tick counter and return the new value.
nextTick :: Connection -> IO Word64
nextTick conn = atomicModifyIORef' (connStmtTick conn) (\n -> (n + 1, n + 1))

ensurePreparedRaw :: Connection -> ByteString -> Vector Word32 -> IO ByteString
ensurePreparedRaw conn sql oids = do
  cache <- readIORef (connStmtCache conn)
  case PSQ.lookup sql cache of
    Just (_prio, name) -> do
      tick <- nextTick conn
      modifyIORef' (connStmtCache conn) (PSQ.insert sql tick name)
      pure name
    Nothing -> do
      evictIfNeeded conn cache
      counter <- atomicModifyIORef' (connStmtCounter conn) (\n -> (n + 1, n))
      let name = "s" <> BS8.pack (show counter)
      resp <- submitRequest (connAsync conn) $ ReqPrepare (Parse name sql oids)
      case resp of
        RespParsed -> do
          tick <- nextTick conn
          modifyIORef' (connStmtCache conn) (PSQ.insert sql tick name)
          pure name
        _ -> throwValiant (ProtocolError "ensurePreparedRaw: unexpected response type")

-- | Evict the least recently used statement if the cache is full.
evictIfNeeded :: Connection -> PSQ.HashPSQ ByteString Word64 ByteString -> IO ()
evictIfNeeded conn cache
  | PSQ.size cache < connMaxPreparedStatements conn = pure ()
  | otherwise = case PSQ.findMin cache of
      Nothing -> pure ()
      Just (oldSql, _prio, oldName) -> do
        resp <- submitRequest (connAsync conn) $ ReqClose (Close DescribeStatement oldName)
        case resp of
          RespClosed -> pure ()
          _ -> pure () -- best effort
        modifyIORef' (connStmtCache conn) (PSQ.delete oldSql)
