-- | Internal: cache entry types for the @valiant-plugin@. The public
-- entry point is "Valiant.Plugin"; this module is exposed only so
-- the plugin's test suite can exercise it. Not stable API.
module Valiant.Plugin.Cache
  ( CacheEntry (..)
  , CacheParam (..)
  , CacheColumn (..)
  , StatementType (..)
  , readCacheEntry
  , findCacheFile
  , findCacheBySqlHash
  , cacheFileName
  ) where

import Data.Aeson (FromJSON (..), eitherDecode, withObject, withText, (.:), (.:?))
import Data.ByteString.Lazy qualified as LBS
import Data.List (find, isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Word (Word32)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- Types (mirrored from Valiant.CLI.Cache) -----------------------------------

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
  , cpPgOid :: Word32
  , cpPgTypeName :: Text
  , cpHaskellType :: Text
  , cpHaskellModule :: Text
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
  }
  deriving stock (Show, Eq)

-- JSON instances ----------------------------------------------------------

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

instance FromJSON StatementType where
  parseJSON = withText "StatementType" $ \case
    "select" -> pure Select
    "insert" -> pure Insert
    "update" -> pure Update
    "delete" -> pure Delete
    _ -> pure Other

instance FromJSON CacheParam where
  parseJSON = withObject "CacheParam" $ \o ->
    CacheParam
      <$> o .: "index"
      <*> o .: "pg_oid"
      <*> o .: "pg_type_name"
      <*> o .: "haskell_type"
      <*> o .: "haskell_module"

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

-- File operations ---------------------------------------------------------

readCacheEntry :: FilePath -> IO (Either String CacheEntry)
readCacheEntry path = eitherDecode <$> LBS.readFile path

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

-- | Find a cache entry by SQL hash alone (for inline SQL without a file path).
-- Scans all cache files in the directory looking for a matching sql_hash.
findCacheBySqlHash :: FilePath -> Text -> IO (Maybe CacheEntry)
findCacheBySqlHash cacheDir sqlHash = do
  exists <- doesDirectoryExist cacheDir
  if not exists
    then pure Nothing
    else do
      files <- listDirectory cacheDir
      let jsonFiles = filter (".json" `isSuffixOf`) files
      go jsonFiles
  where
    go [] = pure Nothing
    go (f : fs) = do
      result <- readCacheEntry (cacheDir </> f)
      case result of
        Right entry | ceSqlHash entry == sqlHash -> pure (Just entry)
        _ -> go fs

cacheFileName :: FilePath -> Text -> FilePath
cacheFileName sqlRelPath hashShort =
  T.unpack (slug <> "-" <> hashShort) <> ".json"
  where
    slug =
      T.replace "/" "-" . T.replace "\\" "-" . T.pack $
        dropSqlExt sqlRelPath
    dropSqlExt p
      | ".sql" `isSuffixOf` p = take (length p - 4) p
      | otherwise = p
