-- | Dynamic SQL statement builder for queries that cannot be expressed
-- as static @.sql@ files.
--
-- 'Snippet' is a composable SQL fragment with embedded typed parameters.
-- Use it for dynamic WHERE clauses, optional filters, dynamic ORDER BY,
-- and other cases where the SQL shape varies at runtime.
--
-- @
-- import Hsqlx.Dynamic
--
-- searchUsers :: Maybe Text -> Maybe Bool -> Connection -> IO [(Int32, Text)]
-- searchUsers mName mActive conn = do
--   let base = sql "SELECT id, name FROM users WHERE true"
--       nameFilter = case mName of
--         Just n  -> sql " AND name ILIKE " <> param n
--         Nothing -> mempty
--       activeFilter = case mActive of
--         Just a  -> sql " AND is_active = " <> param a
--         Nothing -> mempty
--       query = base <> nameFilter <> activeFilter <> sql " ORDER BY name"
--   runSnippet conn query
-- @
module Hsqlx.Dynamic
  ( Snippet
  , sql
  , param
  , runSnippet
  , runSnippetOne
  , snippetToSQL
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word32)
import Hsqlx.FromRow (FromRow (..))
import PgWire.Binary.Types (PgEncode (..))
import PgWire.Connection (Connection)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Oid (Oid (..))
import qualified Hsqlx.Execute as Execute
import Data.Proxy (Proxy (..))

-- | A composable SQL fragment with embedded parameters.
-- 'Snippet' is a 'Monoid', so fragments can be combined with '<>'.
data Snippet = Snippet
  { snipBuilder :: Int -> (Builder, Int)
  -- ^ Given the next parameter index, returns (SQL builder, next index after).
  -- Parameter placeholders are $1, $2, etc.
  , snipParams :: [(Maybe ByteString, Word32)]
  -- ^ Accumulated parameters in reverse order: (encoded value, OID).
  }

instance Semigroup Snippet where
  Snippet b1 p1 <> Snippet b2 p2 = Snippet
    { snipBuilder = \n ->
        let (sql1, n1) = b1 n
            (sql2, n2) = b2 n1
         in (sql1 <> sql2, n2)
    , snipParams = p2 ++ p1
    }
  {-# INLINE (<>) #-}

instance Monoid Snippet where
  mempty = Snippet { snipBuilder = \n -> (mempty, n), snipParams = [] }
  {-# INLINE mempty #-}

-- | A raw SQL fragment with no parameters.
--
-- @
-- sql "SELECT id, name FROM users WHERE "
-- @
sql :: ByteString -> Snippet
sql s = Snippet
  { snipBuilder = \n -> (B.byteString s, n)
  , snipParams = []
  }
{-# INLINE sql #-}

-- | Embed a typed parameter. Generates a @$N@ placeholder and captures
-- the encoded value and OID.
--
-- @
-- sql "WHERE id = " <> param (42 :: Int32)
-- -- produces: "WHERE id = $1" with Int32 parameter
-- @
param :: forall a. (PgEncode a) => a -> Snippet
param a = Snippet
  { snipBuilder = \n ->
      let placeholder = B.byteString ("$" <> BS8.pack (show n))
       in (placeholder, n + 1)
  , snipParams = [(Just (pgEncode a), unOid (pgOid (Proxy @a)))]
  }
  where
    unOid (Oid o) = o
{-# INLINE param #-}

-- | Execute a 'Snippet' and return all result rows.
--
-- @
-- rows <- runSnippet conn (sql "SELECT 1 + " <> param (2 :: Int32))
-- @
runSnippet :: (FromRow r) => Connection -> Snippet -> IO [r]
runSnippet conn snip = do
  let (sqlBs, oids, vals) = materialize snip
  rawRows <- Execute.rawFetchAll conn sqlBs (map fromIntegral oids) vals
  mapM (\row -> case fromRow row of
    Left err -> throwHsqlx (DecodeError (BS8.pack err))
    Right val -> pure val) rawRows

-- | Execute a 'Snippet' and return the first row, or 'Nothing'.
runSnippetOne :: (FromRow r) => Connection -> Snippet -> IO (Maybe r)
runSnippetOne conn snip = do
  let (sqlBs, oids, vals) = materialize snip
  mRow <- Execute.rawFetchOne conn sqlBs (map fromIntegral oids) vals
  case mRow of
    Nothing -> pure Nothing
    Just row -> case fromRow row of
      Left err -> throwHsqlx (DecodeError (BS8.pack err))
      Right val -> pure (Just val)

-- | Render a 'Snippet' to its SQL text (for debugging/logging).
snippetToSQL :: Snippet -> ByteString
snippetToSQL snip =
  let (builder, _) = snipBuilder snip 1
   in LBS.toStrict (B.toLazyByteString builder)

-- Internal: materialize a snippet into SQL + OIDs + values.
materialize :: Snippet -> (ByteString, [Word32], [Maybe ByteString])
materialize snip =
  let (builder, _) = snipBuilder snip 1
      sqlBs = LBS.toStrict (B.toLazyByteString builder)
      paramsOrdered = reverse (snipParams snip)
      oids = map snd paramsOrdered
      vals = map fst paramsOrdered
   in (sqlBs, oids, vals)
