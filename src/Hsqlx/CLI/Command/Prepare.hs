module Hsqlx.CLI.Command.Prepare
  ( runPrepare
  ) where

import Data.Map.Strict qualified as Map
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
import Hsqlx.CLI.NamedParams (NamedParamMapping, preprocessNamedParams)
import Hsqlx.CLI.TypeMap (HaskellType (..), ResolvedType (..), oidToHaskellTypeWith, oidToTypeNameWith, resolveType, resolveUnknownOid)
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
      -- Preprocess :name → $N before sending to Postgres
      let (rewrittenSql, nameMapping) = preprocessNamedParams (sqlContent sqlFile)
          prepFile = sqlFile {sqlContent = rewrittenSql}
      result <- describeQuery conn prepFile
      case result of
        Left err -> do
          printFailed (deMessage err)
          pure False
        Right meta -> do
          nullabilities <- resolveNullability conn (qmColumns meta)
          now <- getCurrentTime
          buildResult <- buildCacheEntry env customs conn sqlFile meta nullabilities nameMapping now
          case buildResult of
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
  -> PQ.Connection
  -> SqlFile
  -> QueryMeta
  -> [Bool]
  -> NamedParamMapping
  -> UTCTime
  -> IO (Either Text CacheEntry)
buildCacheEntry env customs conn sqlFile meta nullabilities nameMapping now = do
  let nameMap = Map.fromList [(idx, name) | (name, idx) <- nameMapping]
  eParams <- resolveParams customs conn nameMap (qmParams meta)
  case eParams of
    Left err -> pure (Left err)
    Right params -> do
      eColumns <- resolveColumns customs conn (zip (qmColumns meta) nullabilities)
      case eColumns of
        Left err -> pure (Left err)
        Right columns -> do
          let sqlText = TE.decodeUtf8 (sqlContent sqlFile)
          pure . Right $
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

resolveParams :: CustomTypeMap -> PQ.Connection -> Map.Map Int Text -> [ParamMeta] -> IO (Either Text [CacheParam])
resolveParams customs conn nameMap = go []
  where
    go !acc [] = pure (Right (reverse acc))
    go !acc (pm : pms) = do
      result <- resolveParam customs conn nameMap pm
      case result of
        Left err -> pure (Left err)
        Right cp -> go (cp : acc) pms

resolveParam :: CustomTypeMap -> PQ.Connection -> Map.Map Int Text -> ParamMeta -> IO (Either Text CacheParam)
resolveParam customs conn nameMap ParamMeta {..} =
  let Oid oid = pmOid
      paramName = Map.lookup pmIndex nameMap
      paramLabel = case paramName of
        Just n -> ":" <> n
        Nothing -> "$" <> T.pack (show pmIndex)
   in case oidToHaskellTypeWith customs pmOid of
        Just ht ->
          pure . Right $
            CacheParam
              { cpIndex = pmIndex
              , cpName = paramName
              , cpPgOid = fromIntegral oid
              , cpPgTypeName = maybe "unknown" id (oidToTypeNameWith mempty pmOid)
              , cpHaskellType = htType ht
              , cpHaskellModule = htModule ht
              , cpPgTypeCategory = Nothing
              , cpPgEnumLabels = Nothing
              }
        Nothing -> do
          -- Auto-discover via pg_type
          resolved <- resolveUnknownOid conn customs pmOid
          case resolved of
            Left err ->
              pure . Left $ err <> " for parameter " <> paramLabel
            Right rt ->
              pure . Right $
                CacheParam
                  { cpIndex = pmIndex
                  , cpName = paramName
                  , cpPgOid = fromIntegral oid
                  , cpPgTypeName = rtPgTypeName rt
                  , cpHaskellType = htType (rtHaskellType rt)
                  , cpHaskellModule = htModule (rtHaskellType rt)
                  , cpPgTypeCategory = rtCategory rt
                  , cpPgEnumLabels = rtEnumLabels rt
                  }

resolveColumns :: CustomTypeMap -> PQ.Connection -> [(ColumnMeta, Bool)] -> IO (Either Text [CacheColumn])
resolveColumns customs conn = go []
  where
    go !acc [] = pure (Right (reverse acc))
    go !acc ((cm, nullable) : rest) = do
      result <- resolveColumn customs conn cm nullable
      case result of
        Left err -> pure (Left err)
        Right cc -> go (cc : acc) rest

resolveColumn :: CustomTypeMap -> PQ.Connection -> ColumnMeta -> Bool -> IO (Either Text CacheColumn)
resolveColumn customs conn ColumnMeta {..} nullable =
  let Oid oid = cmOid
      Oid tOid = cmTableOid
      tOidW32 = fromIntegral tOid :: Word32
      mkColumn ht cat labels =
        CacheColumn
          { ccName = cmName
          , ccPgOid = fromIntegral oid
          , ccPgTypeName = maybe "unknown" id (oidToTypeNameWith mempty cmOid)
          , ccNullable = nullable
          , ccHaskellType = htType (resolveType nullable ht)
          , ccHaskellModule = htModule ht
          , ccSourceTableOid = if tOidW32 == 0 then Nothing else Just tOidW32
          , ccSourceColumnNum = if cmColumnNumber == 0 then Nothing else Just cmColumnNumber
          , ccPgTypeCategory = cat
          , ccPgEnumLabels = labels
          }
   in case oidToHaskellTypeWith customs cmOid of
        Just baseHt ->
          pure . Right $ mkColumn baseHt Nothing Nothing
        Nothing -> do
          resolved <- resolveUnknownOid conn customs cmOid
          case resolved of
            Left err ->
              pure . Left $ err <> " for column \"" <> cmName <> "\""
            Right rt ->
              pure . Right $
                (mkColumn (rtHaskellType rt) (rtCategory rt) (rtEnumLabels rt))
                  { ccPgTypeName = rtPgTypeName rt
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
