module Hsqlx.CLI.Nullability
  ( resolveNullability
  ) where

import Data.ByteString.Char8 qualified as BS8
import Database.PostgreSQL.LibPQ (Oid (..))
import Database.PostgreSQL.LibPQ qualified as PQ
import Hsqlx.CLI.Describe (ColumnMeta (..))

-- | For each column, determine whether it is nullable by querying
-- @pg_attribute@. Columns without a source table (computed columns)
-- are assumed nullable.
resolveNullability :: PQ.Connection -> [ColumnMeta] -> IO [Bool]
resolveNullability conn = mapM (isNullable conn)

-- | Returns @True@ if the column is nullable.
isNullable :: PQ.Connection -> ColumnMeta -> IO Bool
isNullable conn col
  | cmTableOid col == Oid 0 = pure True -- computed / expression column
  | cmColumnNumber col == 0 = pure True -- no source column info
  | otherwise = do
      let Oid tableOid = cmTableOid col
          query =
            "SELECT NOT attnotnull FROM pg_attribute WHERE attrelid = "
              <> BS8.pack (show tableOid)
              <> " AND attnum = "
              <> BS8.pack (show (cmColumnNumber col))
      mResult <- PQ.exec conn query
      case mResult of
        Nothing -> pure True -- assume nullable on error
        Just result -> do
          nRows <- PQ.ntuples result
          if nRows == 0
            then pure True -- no info, assume nullable
            else do
              mVal <- PQ.getvalue result (PQ.toRow (0 :: Int)) (PQ.toColumn (0 :: Int))
              pure $ case mVal of
                Just "t" -> True
                Just "f" -> False
                _ -> True -- default to nullable
