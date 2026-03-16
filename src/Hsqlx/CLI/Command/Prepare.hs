module Hsqlx.CLI.Command.Prepare
  ( runPrepare
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word32)
import Database.PostgreSQL.LibPQ (Oid (..))
import Database.PostgreSQL.LibPQ qualified as PQ
import Hsqlx.CLI.Cache
import Hsqlx.CLI.Config (AppEnv (..))
import Hsqlx.CLI.Describe (ColumnMeta (..), DescribeError (..), ParamMeta (..), QueryMeta (..), describeQuery, withPgConnection)
import Hsqlx.CLI.Discover (SqlFile (..), discoverSqlFiles)
import Hsqlx.CLI.Error (HsqlxCliError (..), dieWithError)
import Hsqlx.CLI.Hash (sha256Hex)
import Hsqlx.CLI.Nullability (resolveNullability)
import Hsqlx.CLI.Output
import Hsqlx.CLI.CustomTypes (CustomTypeMap, customTypesFile, loadCustomTypes)
import Hsqlx.CLI.TypeMap (HaskellType (..), oidToHaskellTypeWith, oidToTypeNameWith, resolveType)
import System.Exit (ExitCode (..))

runPrepare :: AppEnv -> IO ExitCode
runPrepare env = do
  dbUrl <- case appDatabaseUrl env of
    Nothing -> dieWithError ErrNoDatabaseUrl
    Just url -> pure url

  printHeader $ "Using database " <> maskPassword (TE.decodeUtf8 dbUrl)
  printHeader $ "Scanning " <> T.pack (appSqlDir env) <> " for .sql files..."

  files <- discoverSqlFiles (appSqlDir env)
  case files of
    [] -> dieWithError (ErrNoSqlFiles (appSqlDir env))
    _ -> printLn $ "  Found " <> T.pack (show (length files)) <> " queries"

  printLn ""

  customs <- loadCustomTypes customTypesFile
  let total = length files

  withPgConnection dbUrl $ \conn -> do
    results <- mapM (processFile env customs conn total) (zip [1 ..] files)
    let okCount = length (filter id results)
    printSummary okCount total
    printLn $ "  Wrote " <> T.pack (show okCount) <> " cache files to " <> T.pack (appCacheDir env) <> "/"
    pure $
      if okCount == total
        then ExitSuccess
        else ExitFailure 1

processFile :: AppEnv -> CustomTypeMap -> PQ.Connection -> Int -> (Int, SqlFile) -> IO Bool
processFile env customs conn total (idx, sqlFile) = do
  printProgress idx total (sqlRelPath sqlFile)

  -- Check if cache is already current
  existing <- findCacheFile (appCacheDir env) (sqlRelPath sqlFile) (sqlHash sqlFile)
  case existing of
    Just _ -> do
      printOk
      pure True
    Nothing -> do
      result <- describeQuery conn sqlFile
      case result of
        Left err -> do
          printFailed (deMessage err)
          pure False
        Right meta -> do
          nullabilities <- resolveNullability conn (qmColumns meta)
          now <- getCurrentTime
          case buildCacheEntry env customs sqlFile meta nullabilities now of
            Left errMsg -> do
              printFailed errMsg
              pure False
            Right entry -> do
              writeCacheEntry (appCacheDir env) entry
              printOk
              pure True

buildCacheEntry
  :: AppEnv
  -> CustomTypeMap
  -> SqlFile
  -> QueryMeta
  -> [Bool]
  -> UTCTime
  -> Either Text CacheEntry
buildCacheEntry env customs sqlFile meta nullabilities now = do
  params <- mapM (resolveParam customs) (qmParams meta)
  columns <- mapM (uncurry (resolveColumn customs)) (zip (qmColumns meta) nullabilities)
  let sqlText = TE.decodeUtf8 (sqlContent sqlFile)
  Right
    CacheEntry
      { ceVersion = "0.1.0"
      , ceFile = sqlRelPath sqlFile
      , ceSqlHash = sqlHash sqlFile
      , ceSql = sqlText
      , ceDbUrlHash = maybe "" sha256Hex (appDatabaseUrl env)
      , cePreparedAt = now
      , ceStatementType = statementTypeFromSql sqlText
      , ceParams = params
      , ceColumns = columns
      }

resolveParam :: CustomTypeMap -> ParamMeta -> Either Text CacheParam
resolveParam customs ParamMeta {..} =
  let Oid oid = pmOid
   in case oidToHaskellTypeWith customs pmOid of
        Nothing ->
          Left $ "Unknown OID " <> T.pack (show oid) <> " for parameter $" <> T.pack (show pmIndex)
        Just ht ->
          Right
            CacheParam
              { cpIndex = pmIndex
              , cpPgOid = fromIntegral oid
              , cpPgTypeName = maybe "unknown" id (oidToTypeNameWith mempty pmOid)
              , cpHaskellType = htType ht
              , cpHaskellModule = htModule ht
              }

resolveColumn :: CustomTypeMap -> ColumnMeta -> Bool -> Either Text CacheColumn
resolveColumn customs ColumnMeta {..} nullable =
  let Oid oid = cmOid
      Oid tOid = cmTableOid
   in case oidToHaskellTypeWith customs cmOid of
        Nothing ->
          Left $ "Unknown OID " <> T.pack (show oid) <> " for column \"" <> cmName <> "\""
        Just baseHt ->
          let ht = resolveType nullable baseHt
              tOidW32 = fromIntegral tOid :: Word32
           in Right
                CacheColumn
                  { ccName = cmName
                  , ccPgOid = fromIntegral oid
                  , ccPgTypeName = maybe "unknown" id (oidToTypeNameWith mempty cmOid)
                  , ccNullable = nullable
                  , ccHaskellType = htType ht
                  , ccHaskellModule = htModule ht
                  , ccSourceTableOid = if tOidW32 == 0 then Nothing else Just tOidW32
                  , ccSourceColumnNum = if cmColumnNumber == 0 then Nothing else Just cmColumnNumber
                  }

-- | Mask the password in a connection URL for display.
maskPassword :: Text -> Text
maskPassword url =
  case T.breakOn "://" url of
    (scheme, rest)
      | T.null rest -> url
      | otherwise ->
          let afterScheme = T.drop 3 rest
           in case T.breakOn "@" afterScheme of
                (_, hostPart)
                  | T.null hostPart -> url
                  | otherwise ->
                      case T.breakOn ":" afterScheme of
                        (user, passAndHost)
                          | T.null passAndHost -> url
                          | otherwise ->
                              scheme <> "://" <> user <> ":***" <> T.dropWhile (/= '@') passAndHost
