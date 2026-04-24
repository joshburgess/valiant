module Valiant.CLI.Cache
  ( CacheEntry (..)
  , CacheParam (..)
  , CacheColumn (..)
  , StatementType (..)
  , writeCacheEntry
  , readCacheEntry
  , findCacheFile
  , cacheFileName
  , statementTypeFromSql
  , ensureCacheDir
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find, isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Word (Word32)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, renameFile)
import System.FilePath ((</>))

-- | The full cache entry written to a @.valiant/*.json@ file.
data CacheEntry = CacheEntry
  { ceVersion :: Text
  , ceFile :: FilePath
  , ceSqlHash :: Text
  , ceSql :: Text
  , ceDbUrlHash :: Text
  , cePreparedAt :: UTCTime
  , ceStatementType :: StatementType
  , ceParams :: [CacheParam]
  , ceColumns :: [CacheColumn]
  }
  deriving stock (Show, Eq)

data StatementType = Select | Insert | Update | Delete | Other
  deriving stock (Show, Eq)

data CacheParam = CacheParam
  { cpIndex :: Int
  , cpName :: Maybe Text
  -- ^ Named parameter name from @:name@ syntax. 'Nothing' for positional @$N@.
  , cpPgOid :: Word32
  , cpPgTypeName :: Text
  , cpHaskellType :: Text
  , cpHaskellModule :: Text
  , cpPgTypeCategory :: Maybe Text
  -- ^ @\"enum\"@, @\"domain\"@, @\"range\"@, etc. 'Nothing' for built-in types.
  , cpPgEnumLabels :: Maybe [Text]
  -- ^ Enum labels from @pg_enum@, if this is an enum type.
  }
  deriving stock (Show, Eq)

data CacheColumn = CacheColumn
  { ccName :: Text
  , ccPgOid :: Word32
  , ccPgTypeName :: Text
  , ccNullable :: Bool
  , ccHaskellType :: Text
  , ccHaskellModule :: Text
  , ccSourceTableOid :: Maybe Word32
  , ccSourceColumnNum :: Maybe Int
  , ccPgTypeCategory :: Maybe Text
  -- ^ @\"enum\"@, @\"domain\"@, @\"range\"@, etc. 'Nothing' for built-in types.
  , ccPgEnumLabels :: Maybe [Text]
  -- ^ Enum labels from @pg_enum@, if this is an enum type.
  }
  deriving stock (Show, Eq)

-- JSON instances ----------------------------------------------------------

instance ToJSON CacheEntry where
  toJSON CacheEntry {..} =
    object
      [ "valiant_version" .= ceVersion
      , "file" .= ceFile
      , "sql_hash" .= ceSqlHash
      , "sql" .= ceSql
      , "database_url_hash" .= ceDbUrlHash
      , "prepared_at" .= cePreparedAt
      , "statement_type" .= ceStatementType
      , "parameters" .= ceParams
      , "columns" .= ceColumns
      ]

instance FromJSON CacheEntry where
  parseJSON = withObject "CacheEntry" $ \o ->
    CacheEntry
      <$> o .: "valiant_version"
      <*> o .: "file"
      <*> o .: "sql_hash"
      <*> o .: "sql"
      <*> o .: "database_url_hash"
      <*> o .: "prepared_at"
      <*> o .: "statement_type"
      <*> o .: "parameters"
      <*> o .: "columns"

instance ToJSON StatementType where
  toJSON = \case
    Select -> String "select"
    Insert -> String "insert"
    Update -> String "update"
    Delete -> String "delete"
    Other -> String "other"

instance FromJSON StatementType where
  parseJSON = withText "StatementType" $ \case
    "select" -> pure Select
    "insert" -> pure Insert
    "update" -> pure Update
    "delete" -> pure Delete
    _ -> pure Other

instance ToJSON CacheParam where
  toJSON CacheParam {..} =
    object
      [ "index" .= cpIndex
      , "name" .= cpName
      , "pg_oid" .= cpPgOid
      , "pg_type_name" .= cpPgTypeName
      , "haskell_type" .= cpHaskellType
      , "haskell_module" .= cpHaskellModule
      , "pg_type_category" .= cpPgTypeCategory
      , "pg_enum_labels" .= cpPgEnumLabels
      ]

instance FromJSON CacheParam where
  parseJSON = withObject "CacheParam" $ \o ->
    CacheParam
      <$> o .: "index"
      <*> o .:? "name"
      <*> o .: "pg_oid"
      <*> o .: "pg_type_name"
      <*> o .: "haskell_type"
      <*> o .: "haskell_module"
      <*> o .:? "pg_type_category"
      <*> o .:? "pg_enum_labels"

instance ToJSON CacheColumn where
  toJSON CacheColumn {..} =
    object
      [ "name" .= ccName
      , "pg_oid" .= ccPgOid
      , "pg_type_name" .= ccPgTypeName
      , "nullable" .= ccNullable
      , "haskell_type" .= ccHaskellType
      , "haskell_module" .= ccHaskellModule
      , "source_table_oid" .= ccSourceTableOid
      , "source_column_number" .= ccSourceColumnNum
      , "pg_type_category" .= ccPgTypeCategory
      , "pg_enum_labels" .= ccPgEnumLabels
      ]

instance FromJSON CacheColumn where
  parseJSON = withObject "CacheColumn" $ \o ->
    CacheColumn
      <$> o .: "name"
      <*> o .: "pg_oid"
      <*> o .: "pg_type_name"
      <*> o .: "nullable"
      <*> o .: "haskell_type"
      <*> o .: "haskell_module"
      <*> o .:? "source_table_oid"
      <*> o .:? "source_column_number"
      <*> o .:? "pg_type_category"
      <*> o .:? "pg_enum_labels"

-- File operations ---------------------------------------------------------

-- | Ensure the cache directory exists.
ensureCacheDir :: FilePath -> IO ()
ensureCacheDir = createDirectoryIfMissing True

-- | Write a 'CacheEntry' to disk as pretty-printed JSON.
-- Uses write-to-temp-then-rename for atomicity: a crash mid-write
-- cannot leave a corrupted cache file.
writeCacheEntry :: FilePath -> CacheEntry -> IO ()
writeCacheEntry cacheDir entry = do
  ensureCacheDir cacheDir
  let fileName = cacheFileName (ceFile entry) (T.take 12 (ceSqlHash entry))
      path = cacheDir </> fileName
      tmpPath = path <> ".tmp"
  LBS.writeFile tmpPath (encodePretty entry)
  renameFile tmpPath path

-- | Read a 'CacheEntry' from a JSON file.
readCacheEntry :: FilePath -> IO (Either String CacheEntry)
readCacheEntry path = do
  bytes <- LBS.readFile path
  pure (eitherDecode bytes)

-- | Find a cache file matching the given SQL file path and content hash.
findCacheFile :: FilePath -> FilePath -> Text -> IO (Maybe CacheEntry)
findCacheFile cacheDir sqlRelPath sqlHash = do
  exists <- doesDirectoryExist cacheDir
  if not exists
    then pure Nothing
    else do
      files <- listDirectory cacheDir
      let hashShort = T.take 12 sqlHash
          expected = cacheFileName sqlRelPath hashShort
      case find (== expected) files of
        Nothing -> pure Nothing
        Just f -> do
          result <- readCacheEntry (cacheDir </> f)
          pure $ case result of
            Left _ -> Nothing
            Right entry
              | ceSqlHash entry == sqlHash -> Just entry
              | otherwise -> Nothing

-- | Generate a cache file name from an SQL relative path and truncated hash.
--
-- >>> cacheFileName "users/find_by_id.sql" "a1b2c3d4e5f6"
-- "users-find_by_id-a1b2c3d4e5f6.json"
cacheFileName :: FilePath -> Text -> FilePath
cacheFileName sqlRelPath hashShort =
  T.unpack (slug <> "-" <> hashShort) <> ".json"
  where
    slug =
      T.replace "/" "-" . T.replace "\\" "-" . T.pack $
        dropSqlExt sqlRelPath

    dropSqlExt "<inline>" = "inline"
    dropSqlExt p
      | ".sql" `isSuffixOf` p = take (length p - 4) p
      | otherwise = p

-- | Infer the statement type from the SQL text.
statementTypeFromSql :: Text -> StatementType
statementTypeFromSql sql =
  case T.words (T.toCaseFold (T.stripStart sql)) of
    (w : _)
      | w == "select" -> Select
      | w == "insert" -> Insert
      | w == "update" -> Update
      | w == "delete" -> Delete
      | w == "with" -> Select -- CTEs are usually selects
    _ -> Other
