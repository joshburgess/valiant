module Valiant.CLI.Describe
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

import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Word (Word32)
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection, close, connAsync, connectString, simpleQuery)
import PgWire.Error (PgWireError)
import PgWire.Protocol.Backend (BackendMsg (..), FieldInfo (..), PgError (..), TxStatus)
import PgWire.Protocol.Frontend (DescribeTarget (..), FrontendMsg (..))
import PgWire.Protocol.Oid (Oid (..))
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg)
import Valiant.CLI.Discover (SqlFile (..))

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
withPgConnection :: ByteString -> (Connection -> IO a) -> IO a
withPgConnection connStr action = do
  result <- try @PgWireError (bracket (connectString connStr) close action)
  case result of
    Right a -> pure a
    Left err -> error $ "valiant: connection failed: " <> show err

-- | Prepare and describe a SQL query, returning structured metadata.
describeQuery :: Connection -> SqlFile -> IO (Either DescribeError QueryMeta)
describeQuery conn sqlFile = do
  let stmtName = BS8.pack ("valiant_" <> sqlRelPath sqlFile)
      sql = sqlContent sqlFile
  result <- parseAndDescribe conn stmtName sql
  pure $ case result of
    Left err -> Left (pgErrorToDescribeError err)
    Right (paramOids, fields) -> Right (buildMeta paramOids fields)

-- | Send Parse + Describe + Sync in one round trip and collect the response.
-- Returns the parameter OIDs and column field info, or the server's error.
parseAndDescribe
  :: Connection
  -> ByteString
  -- ^ statement name
  -> ByteString
  -- ^ SQL text
  -> IO (Either PgError (Vector Word32, Vector FieldInfo))
parseAndDescribe conn stmtName sql =
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsg wc (Parse stmtName sql V.empty)
    sendFrontendMsg wc (Describe DescribeStatement stmtName)
    sendFrontendMsg wc Sync
    collectDescribeResponse wc txRef

-- | Collect the response to a Parse + Describe + Sync sequence.
-- Expected message order on success:
-- ParseComplete, ParameterDescription, (RowDescription | NoData), ReadyForQuery.
-- On error: ErrorResponse, ReadyForQuery.
collectDescribeResponse
  :: WireConn
  -> IORef TxStatus
  -> IO (Either PgError (Vector Word32, Vector FieldInfo))
collectDescribeResponse wc txRef = loop Nothing V.empty V.empty
  where
    loop !mErr !paramOids !fields = do
      msg <- recvBackendMsg wc
      case msg of
        ParseComplete -> loop mErr paramOids fields
        ParameterDescription oids -> loop mErr oids fields
        RowDescription fs -> loop mErr paramOids fs
        NoData -> loop mErr paramOids V.empty
        ErrorResponse err -> loop (Just err) paramOids fields
        ReadyForQuery status -> do
          writeIORef txRef status
          pure $ case mErr of
            Just err -> Left err
            Nothing -> Right (paramOids, fields)
        _ -> loop mErr paramOids fields

buildMeta :: Vector Word32 -> Vector FieldInfo -> QueryMeta
buildMeta paramOids fields =
  QueryMeta
    { qmParams =
        [ ParamMeta {pmIndex = i + 1, pmOid = Oid oid}
        | (i, oid) <- zip [0 ..] (V.toList paramOids)
        ]
    , qmColumns = map fieldToColumnMeta (V.toList fields)
    }

fieldToColumnMeta :: FieldInfo -> ColumnMeta
fieldToColumnMeta fi =
  ColumnMeta
    { cmName = TE.decodeUtf8 (fiName fi)
    , cmOid = Oid (fiTypeOid fi)
    , cmTableOid = Oid (fiTableOid fi)
    , cmColumnNumber = fromIntegral (fiColumnNum fi)
    }

pgErrorToDescribeError :: PgError -> DescribeError
pgErrorToDescribeError err =
  DescribeError
    { deMessage = TE.decodeUtf8 (pgMessage err)
    , deDetail = TE.decodeUtf8 <$> pgDetail err
    , deHint = TE.decodeUtf8 <$> pgHint err
    }

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
queryTypeInfo :: Connection -> Oid -> IO (Maybe PgTypeInfo)
queryTypeInfo conn (Oid rawOid) = do
  let sql =
        "SELECT typname, typtype, typarray, typbasetype, typelem FROM pg_type WHERE oid = "
          <> BS8.pack (show rawOid)
  result <- try @PgWireError (simpleQuery conn sql)
  pure $ case result of
    Left _ -> Nothing
    Right (rows, _) -> case rows of
      (row : _) -> Just (parsePgTypeRow row)
      [] -> Nothing

parsePgTypeRow :: [Maybe ByteString] -> PgTypeInfo
parsePgTypeRow row =
  let (mName, mTyptype, mTyparray, mTypbase, mTypelem) = case row of
        (a : b : c : d : e : _) -> (a, b, c, d, e)
        _ -> (Nothing, Nothing, Nothing, Nothing, Nothing)
   in PgTypeInfo
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
queryEnumLabels :: Connection -> Oid -> IO [Text]
queryEnumLabels conn (Oid rawOid) = do
  let sql =
        "SELECT enumlabel FROM pg_enum WHERE enumtypid = "
          <> BS8.pack (show rawOid)
          <> " ORDER BY enumsortorder"
  result <- try @PgWireError (simpleQuery conn sql)
  pure $ case result of
    Left _ -> []
    Right (rows, _) -> map labelOf rows
  where
    labelOf (Just v : _) = TE.decodeUtf8 v
    labelOf _ = ""

-- | Query @pg_range@ for the subtype OID of a range type.
queryRangeSubtype :: Connection -> Oid -> IO (Maybe Oid)
queryRangeSubtype conn (Oid rawOid) = do
  let sql =
        "SELECT rngsubtype FROM pg_range WHERE rngtypid = "
          <> BS8.pack (show rawOid)
  result <- try @PgWireError (simpleQuery conn sql)
  pure $ case result of
    Left _ -> Nothing
    Right (rows, _) -> case rows of
      ((Just v : _) : _) -> case BS8.readInt v of
        Just (n, _) -> Just (Oid (fromIntegral n))
        Nothing -> Nothing
      _ -> Nothing
