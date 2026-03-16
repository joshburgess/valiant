{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.FromRow
  ( FromRow (..)
  ) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Types (PgDecode (..))

-- | Decode a result row into a Haskell value.
class FromRow a where
  fromRow :: Vector (Maybe ByteString) -> Either String a

-- Unit instance for commands (INSERT/UPDATE/DELETE) that return no rows
instance FromRow () where
  fromRow _ = Right ()

-- Single non-nullable column
instance {-# OVERLAPPABLE #-} (PgDecode a) => FromRow a where
  fromRow row = decodeCol row 0

-- Single-value instance for scalar queries
instance (PgDecode a) => FromRow (Only a) where
  fromRow row = do
    a <- decodeCol row 0
    pure (Only a)

-- | Wrapper for single-column results.
newtype Only a = Only {fromOnly :: a}
  deriving stock (Show, Eq)

-- Tuple instances ---------------------------------------------------------

instance (PgDecode a, PgDecode b) => FromRow (a, b) where
  fromRow row = do
    a <- decodeCol row 0
    b <- decodeCol row 1
    pure (a, b)

instance (PgDecode a, PgDecode b, PgDecode c) => FromRow (a, b, c) where
  fromRow row = do
    a <- decodeCol row 0
    b <- decodeCol row 1
    c <- decodeCol row 2
    pure (a, b, c)

instance (PgDecode a, PgDecode b, PgDecode c, PgDecode d) => FromRow (a, b, c, d) where
  fromRow row = do
    a <- decodeCol row 0
    b <- decodeCol row 1
    c <- decodeCol row 2
    d <- decodeCol row 3
    pure (a, b, c, d)

instance (PgDecode a, PgDecode b, PgDecode c, PgDecode d, PgDecode e) => FromRow (a, b, c, d, e) where
  fromRow row = do
    a <- decodeCol row 0
    b <- decodeCol row 1
    c <- decodeCol row 2
    d <- decodeCol row 3
    e <- decodeCol row 4
    pure (a, b, c, d, e)

instance (PgDecode a, PgDecode b, PgDecode c, PgDecode d, PgDecode e, PgDecode f) => FromRow (a, b, c, d, e, f) where
  fromRow row = do
    a <- decodeCol row 0
    b <- decodeCol row 1
    c <- decodeCol row 2
    d <- decodeCol row 3
    e <- decodeCol row 4
    f <- decodeCol row 5
    pure (a, b, c, d, e, f)

-- Maybe instance for nullable results
instance (PgDecode a) => PgDecode (Maybe a) where
  pgDecode bs = Just <$> pgDecode bs
  {-# INLINE pgDecode #-}

-- Helpers -----------------------------------------------------------------

decodeCol :: (PgDecode a) => Vector (Maybe ByteString) -> Int -> Either String a
decodeCol row idx
  | idx >= V.length row = Left $ "Column index " <> show idx <> " out of range (row has " <> show (V.length row) <> " columns)"
  | otherwise = case row V.! idx of
      Nothing -> Left $ "Column " <> show idx <> " is NULL but expected a non-nullable value"
      Just bs -> pgDecode bs
{-# INLINE decodeCol #-}
