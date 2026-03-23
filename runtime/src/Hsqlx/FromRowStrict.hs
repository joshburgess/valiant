{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Strict variant of 'FromRow' that validates column count.
--
-- 'FromRow' silently ignores extra columns in the result row. This is
-- convenient but can mask schema evolution bugs. 'FromRowStrict' checks
-- that the number of columns matches exactly, failing with an error if
-- there are extra or missing columns.
--
-- @
-- data User = User { userId :: Int32, userName :: Text }
--   deriving stock (Generic)
--   deriving anyclass (FromRowStrict)
--
-- -- SELECT id, name FROM users         → OK
-- -- SELECT id, name, email FROM users  → Error: expected 2 columns, got 3
-- @
module Hsqlx.FromRowStrict
  ( FromRowStrict (..)
  ) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Hsqlx.FromRow (DecodeColumn (..), GFromRow (..))

-- | Like 'FromRow' but validates that the row has exactly the expected
-- number of columns. Derive via @Generic@ the same way as 'FromRow'.
class FromRowStrict a where
  fromRowStrict :: Vector (Maybe ByteString) -> Either String a
  default fromRowStrict :: (Generic a, GFromRow (Rep a)) => Vector (Maybe ByteString) -> Either String a
  fromRowStrict row
    | V.length row /= expected =
        Left $ "Column count mismatch: expected " <> show expected
            <> " but got " <> show (V.length row)
    | otherwise = to <$> gFromRow row 0
    where
      expected = gFieldCount (undefined :: proxy (Rep a))

-- Tuple instances with exact column count validation.

instance (DecodeColumn a, DecodeColumn b) => FromRowStrict (a, b) where
  fromRowStrict row
    | V.length row /= 2 = Left $ "Column count mismatch: expected 2 but got " <> show (V.length row)
    | otherwise = (,) <$> decodeColumn row 0 <*> decodeColumn row 1

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c) => FromRowStrict (a, b, c) where
  fromRowStrict row
    | V.length row /= 3 = Left $ "Column count mismatch: expected 3 but got " <> show (V.length row)
    | otherwise = (,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d) => FromRowStrict (a, b, c, d) where
  fromRowStrict row
    | V.length row /= 4 = Left $ "Column count mismatch: expected 4 but got " <> show (V.length row)
    | otherwise = (,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e) => FromRowStrict (a, b, c, d, e) where
  fromRowStrict row
    | V.length row /= 5 = Left $ "Column count mismatch: expected 5 but got " <> show (V.length row)
    | otherwise = (,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4
