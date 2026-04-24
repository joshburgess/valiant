module Valiant.CLI.Nullability
  ( resolveNullability
  ) where

import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import PgWire.Connection (Connection, simpleQuery)
import PgWire.Error (PgWireError)
import PgWire.Protocol.Oid (Oid (..))
import Valiant.CLI.Describe (ColumnMeta (..))

-- | For each column, determine whether it is nullable by querying
-- @pg_attribute@. Columns without a source table (computed columns)
-- are assumed nullable.
resolveNullability :: Connection -> [ColumnMeta] -> IO [Bool]
resolveNullability conn = mapM (isNullable conn)

-- | Returns @True@ if the column is nullable.
isNullable :: Connection -> ColumnMeta -> IO Bool
isNullable conn col
  | cmTableOid col == Oid 0 = pure True -- computed / expression column
  | cmColumnNumber col == 0 = pure True -- no source column info
  | otherwise = do
      let Oid tableOid = cmTableOid col
          sql =
            "SELECT NOT attnotnull FROM pg_attribute WHERE attrelid = "
              <> BS8.pack (show tableOid)
              <> " AND attnum = "
              <> BS8.pack (show (cmColumnNumber col))
      result <- try @PgWireError (simpleQuery conn sql)
      pure $ case result of
        Left _ -> True -- assume nullable on error
        Right (rows, _) -> case rows of
          ((Just "t" : _) : _) -> True
          ((Just "f" : _) : _) -> False
          _ -> True -- no info or unexpected value: default to nullable
