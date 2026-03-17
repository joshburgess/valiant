-- | Pool-level cache for resolved PostgreSQL type metadata.
--
-- When multiple connections encounter the same custom OID (e.g., a PG
-- enum), this cache avoids redundant @pg_type@ round-trips. The cache
-- is shared across all connections in a pool via a 'TVar'.
module PgWire.TypeCache
  ( TypeCache
  , TypeInfo (..)
  , newTypeCache
  , lookupTypeInfo
  , cacheTypeInfo
  , cachedTypeCount
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)

-- | Metadata about a PG type, cached at the pool level.
data TypeInfo = TypeInfo
  { tiName :: !ByteString
  -- ^ Type name from @pg_type.typname@.
  , tiCategory :: !Char
  -- ^ Type category: @\'E\'@ enum, @\'C\'@ composite, @\'R\'@ range,
  --   @\'D\'@ domain, @\'B\'@ base, @\'P\'@ pseudo, etc.
  , tiArrayOid :: !Word32
  -- ^ OID of the array form of this type (0 if none).
  , tiBaseOid :: !Word32
  -- ^ For domains: the underlying type OID. 0 otherwise.
  }
  deriving stock (Show, Eq)

-- | A thread-safe cache mapping OIDs to their resolved type info.
newtype TypeCache = TypeCache (TVar (Map Word32 TypeInfo))

-- | Create an empty type cache.
newTypeCache :: IO TypeCache
newTypeCache = TypeCache <$> newTVarIO Map.empty

-- | Look up cached type info for an OID.
lookupTypeInfo :: TypeCache -> Word32 -> IO (Maybe TypeInfo)
lookupTypeInfo (TypeCache tv) oid = do
  m <- readTVarIO tv
  pure (Map.lookup oid m)

-- | Cache type info for an OID.
cacheTypeInfo :: TypeCache -> Word32 -> TypeInfo -> IO ()
cacheTypeInfo (TypeCache tv) oid info =
  atomically $ modifyTVar' tv (Map.insert oid info)

-- | Return the number of cached entries (for diagnostics).
cachedTypeCount :: TypeCache -> IO Int
cachedTypeCount (TypeCache tv) = Map.size <$> readTVarIO tv
