{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Decode result rows into Haskell types.
module Hsqlx.FromRow
  ( FromRow (..)
  , DecodeColumn (..)
  ) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Types (PgDecode (..))

-- | Decode a single column value, handling NULL appropriately.
--
-- For non-Maybe types, NULL produces an error.
-- For @Maybe a@, NULL produces @Nothing@.
class DecodeColumn a where
  decodeColumn :: Vector (Maybe ByteString) -> Int -> Either String a

-- | Non-nullable column: NULL is an error.
instance {-# OVERLAPPABLE #-} (PgDecode a) => DecodeColumn a where
  decodeColumn row idx
    | idx >= V.length row = Left $ "Column index " <> show idx <> " out of range (row has " <> show (V.length row) <> " columns)"
    | otherwise = case row V.! idx of
        Nothing -> Left $ "Column " <> show idx <> " is NULL but expected a non-nullable value"
        Just bs -> pgDecode bs
  {-# INLINE decodeColumn #-}

-- | Nullable column: NULL becomes @Nothing@, non-NULL becomes @Just a@.
instance (PgDecode a) => DecodeColumn (Maybe a) where
  decodeColumn row idx
    | idx >= V.length row = Left $ "Column index " <> show idx <> " out of range (row has " <> show (V.length row) <> " columns)"
    | otherwise = case row V.! idx of
        Nothing -> Right Nothing
        Just bs -> Just <$> pgDecode bs
  {-# INLINE decodeColumn #-}

-- | Decode a result row into a Haskell value.
class FromRow a where
  fromRow :: Vector (Maybe ByteString) -> Either String a

-- Unit instance for commands (INSERT/UPDATE/DELETE) that return no rows
instance FromRow () where
  fromRow _ = Right ()

-- Single non-nullable column
instance {-# OVERLAPPABLE #-} (DecodeColumn a) => FromRow a where
  fromRow row = decodeColumn row 0

-- Single-value instance for scalar queries
instance (DecodeColumn a) => FromRow (Only a) where
  fromRow row = do
    a <- decodeColumn row 0
    pure (Only a)

-- | Wrapper for single-column results.
newtype Only a = Only {fromOnly :: a}
  deriving stock (Show, Eq)

-- Tuple instances ---------------------------------------------------------

instance (DecodeColumn a, DecodeColumn b) => FromRow (a, b) where
  fromRow row = do
    a <- decodeColumn row 0
    b <- decodeColumn row 1
    pure (a, b)

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c) => FromRow (a, b, c) where
  fromRow row = do
    a <- decodeColumn row 0
    b <- decodeColumn row 1
    c <- decodeColumn row 2
    pure (a, b, c)

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d) => FromRow (a, b, c, d) where
  fromRow row = do
    a <- decodeColumn row 0
    b <- decodeColumn row 1
    c <- decodeColumn row 2
    d <- decodeColumn row 3
    pure (a, b, c, d)

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e) => FromRow (a, b, c, d, e) where
  fromRow row = do
    a <- decodeColumn row 0
    b <- decodeColumn row 1
    c <- decodeColumn row 2
    d <- decodeColumn row 3
    e <- decodeColumn row 4
    pure (a, b, c, d, e)

instance (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f) => FromRow (a, b, c, d, e, f) where
  fromRow row = do
    a <- decodeColumn row 0
    b <- decodeColumn row 1
    c <- decodeColumn row 2
    d <- decodeColumn row 3
    e <- decodeColumn row 4
    f <- decodeColumn row 5
    pure (a, b, c, d, e, f)

-- Maybe instance for PgDecode (used by the single-column overlappable FromRow)
instance (PgDecode a) => PgDecode (Maybe a) where
  pgDecode bs = Just <$> pgDecode bs
  {-# INLINE pgDecode #-}
