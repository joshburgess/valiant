module Hsqlx.CLI.Describe
  ( QueryMeta (..)
  , ParamMeta (..)
  , ColumnMeta (..)
  , DescribeError (..)
  , PgTypeInfo (..)
  , PgTypeCategory (..)
  , describeQuery
  , queryTypeInfo
  , queryEnumLabels
  , queryRangeSubtype
  , withPgConnection
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Database.PostgreSQL.LibPQ (Oid (..))
import Database.PostgreSQL.LibPQ qualified as PQ
import Foreign.C.Types (CUInt)
import Hsqlx.CLI.Discover (SqlFile (..))

-- | Raw metadata returned by Postgres for a described query.
data QueryMeta = QueryMeta
  { qmParams :: [ParamMeta]
  , qmColumns :: [ColumnMeta]
  }
  deriving stock (Show)

-- | Metadata for a single query parameter.
data ParamMeta = ParamMeta
  { pmIndex :: Int
  , pmOid :: Oid
  }
  deriving stock (Show)

-- | Metadata for a single result column.
data ColumnMeta = ColumnMeta
  { cmName :: Text
  , cmOid :: Oid
  , cmTableOid :: Oid
  , cmColumnNumber :: Int
  }
  deriving stock (Show)

-- | An error that occurred while describing a query.
data DescribeError = DescribeError
  { deMessage :: Text
  , deDetail :: Maybe Text
  , deHint :: Maybe Text
  }
  deriving stock (Show)

-- | Connect to Postgres, run an action, and close the connection.
withPgConnection :: ByteString -> (PQ.Connection -> IO a) -> IO a
withPgConnection connStr action = do
  conn <- PQ.connectdb connStr
  status <- PQ.status conn
  case status of
    PQ.ConnectionOk -> do
      result <- action conn
      PQ.finish conn
      pure result
    _ -> do
      msg <- maybe "unknown error" TE.decodeUtf8 <$> PQ.errorMessage conn
      PQ.finish conn
      error $ "hsqlx: connection failed: " <> show msg

-- | Prepare and describe a SQL query, returning structured metadata.
describeQuery :: PQ.Connection -> SqlFile -> IO (Either DescribeError QueryMeta)
describeQuery conn sqlFile = do
  let stmtName = BS8.pack ("hsqlx_" <> sqlRelPath sqlFile)
      sql = sqlContent sqlFile

  -- Prepare the statement
  mResult <- PQ.prepare conn stmtName sql Nothing
  case mResult of
    Nothing -> pure . Left $ DescribeError "PQprepare returned null" Nothing Nothing
    Just result -> do
      st <- PQ.resultStatus result
      case st of
        PQ.CommandOk -> describeStmt conn stmtName
        _ -> Left <$> extractPgError result

describeStmt :: PQ.Connection -> ByteString -> IO (Either DescribeError QueryMeta)
describeStmt conn stmtName = do
  mResult <- PQ.describePrepared conn stmtName
  case mResult of
    Nothing -> pure . Left $ DescribeError "PQdescribePrepared returned null" Nothing Nothing
    Just result -> do
      st <- PQ.resultStatus result
      case st of
        PQ.CommandOk -> Right <$> extractMeta result
        _ -> Left <$> extractPgError result

extractMeta :: PQ.Result -> IO QueryMeta
extractMeta result = do
  nParams <- PQ.nparams result
  params <- mapM (extractParam result) [0 .. nParams - 1]
  nFields <- PQ.nfields result
  let PQ.Col nf = nFields
      fieldRange = map PQ.Col [0 .. nf - 1]
  columns <- mapM (extractColumn result) fieldRange
  pure QueryMeta {qmParams = params, qmColumns = columns}

extractParam :: PQ.Result -> Int -> IO ParamMeta
extractParam result idx = do
  oid <- PQ.paramtype result idx
  pure ParamMeta {pmIndex = idx + 1, pmOid = oid}

extractColumn :: PQ.Result -> PQ.Column -> IO ColumnMeta
extractColumn result col = do
  mName <- PQ.fname result col
  let name = maybe "?" TE.decodeUtf8 mName
  oid <- PQ.ftype result col
  tableOid <- PQ.ftable result col
  colNum <- PQ.ftablecol result col
  let PQ.Col colNumInt = colNum
  pure
    ColumnMeta
      { cmName = name
      , cmOid = oid
      , cmTableOid = tableOid
      , cmColumnNumber = fromIntegral colNumInt
      }

extractPgError :: PQ.Result -> IO DescribeError
extractPgError result = do
  msg <- maybe "unknown error" TE.decodeUtf8 <$> PQ.resultErrorField result PQ.DiagMessagePrimary
  detail <- fmap TE.decodeUtf8 <$> PQ.resultErrorField result PQ.DiagMessageDetail
  hint <- fmap TE.decodeUtf8 <$> PQ.resultErrorField result PQ.DiagMessageHint
  pure DescribeError {deMessage = msg, deDetail = detail, deHint = hint}

-- Type discovery -------------------------------------------------------------

-- | Category of a PG type discovered from @pg_type@.
data PgTypeCategory
  = PgEnum
  | PgComposite
  | PgDomain
  | PgRange
  | PgBase
  | PgPseudo
  deriving stock (Show, Eq)

-- | Metadata about a PG type, queried from @pg_type@.
data PgTypeInfo = PgTypeInfo
  { ptiName :: Text
  , ptiCategory :: PgTypeCategory
  , ptiArrayOid :: Word32
  -- ^ OID of the array form of this type (0 if none).
  , ptiBaseOid :: Word32
  -- ^ For domains: the underlying type OID. 0 otherwise.
  , ptiElemOid :: Word32
  -- ^ For array types: the element type OID. 0 otherwise.
  }
  deriving stock (Show)

-- | Query @pg_type@ for information about an unknown OID.
queryTypeInfo :: PQ.Connection -> Oid -> IO (Maybe PgTypeInfo)
queryTypeInfo conn (Oid rawOid) = do
  let oidParam = BS8.pack (show (fromIntegral rawOid :: Word32))
  mResult <- PQ.execParams conn
    "SELECT typname, typtype, typarray, typbasetype, typelem FROM pg_type WHERE oid = $1"
    [Just (PQ.Oid 26, oidParam, PQ.Text)]  -- OID 26 = oid type
    PQ.Text
  case mResult of
    Nothing -> pure Nothing
    Just result -> do
      nRows <- PQ.ntuples result
      if nRows == 0
        then pure Nothing
        else do
          mName <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (0 :: Int))
          mTyptype <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (1 :: Int))
          mTyparray <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (2 :: Int))
          mTypbase <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (3 :: Int))
          mTypelem <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (4 :: Int))
          pure $
            Just
              PgTypeInfo
                { ptiName = maybe "" TE.decodeUtf8 mName
                , ptiCategory = parseTyptype mTyptype
                , ptiArrayOid = parseOidField mTyparray
                , ptiBaseOid = parseOidField mTypbase
                , ptiElemOid = parseOidField mTypelem
                }

parseTyptype :: Maybe ByteString -> PgTypeCategory
parseTyptype (Just "e") = PgEnum
parseTyptype (Just "c") = PgComposite
parseTyptype (Just "d") = PgDomain
parseTyptype (Just "r") = PgRange
parseTyptype (Just "p") = PgPseudo
parseTyptype _ = PgBase

parseOidField :: Maybe ByteString -> Word32
parseOidField Nothing = 0
parseOidField (Just bs) =
  case BS8.readInt bs of
    Just (n, _) -> fromIntegral n
    Nothing -> 0

-- | Query @pg_enum@ for the labels of an enum type.
queryEnumLabels :: PQ.Connection -> Oid -> IO [Text]
queryEnumLabels conn (Oid rawOid) = do
  let oidParam = BS8.pack (show (fromIntegral rawOid :: Word32))
  mResult <- PQ.execParams conn
    "SELECT enumlabel FROM pg_enum WHERE enumtypid = $1 ORDER BY enumsortorder"
    [Just (PQ.Oid 26, oidParam, PQ.Text)]
    PQ.Text
  case mResult of
    Nothing -> pure []
    Just result -> do
      nRows <- PQ.ntuples result
      let rows = [0 .. nRows - 1]
      mapM
        ( \r -> do
            mVal <- PQ.getvalue result r (PQ.toColumn (0 :: Int))
            pure (maybe "" TE.decodeUtf8 mVal)
        )
        rows

-- | Query @pg_range@ for the subtype OID of a range type.
queryRangeSubtype :: PQ.Connection -> Oid -> IO (Maybe Oid)
queryRangeSubtype conn (Oid rawOid) = do
  let oidParam = BS8.pack (show (fromIntegral rawOid :: Word32))
  mResult <- PQ.execParams conn
    "SELECT rngsubtype FROM pg_range WHERE rngtypid = $1"
    [Just (PQ.Oid 26, oidParam, PQ.Text)]
    PQ.Text
  case mResult of
    Nothing -> pure Nothing
    Just result -> do
      nRows <- PQ.ntuples result
      if nRows == 0
        then pure Nothing
        else do
          mVal <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (0 :: Int))
          pure $ case mVal >>= fmap fst . BS8.readInt of
            Just n -> Just (Oid (fromIntegral n :: CUInt))
            Nothing -> Nothing
