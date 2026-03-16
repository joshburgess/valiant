module Hsqlx.CLI.Describe
  ( QueryMeta (..)
  , ParamMeta (..)
  , ColumnMeta (..)
  , DescribeError (..)
  , describeQuery
  , withPgConnection
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.LibPQ (Oid)
import Database.PostgreSQL.LibPQ qualified as PQ
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
