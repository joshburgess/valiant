{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Decode result rows into Haskell types.
module Hsqlx.FromRow
  ( FromRow (..)
  , DecodeColumn (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Types (PgDecode (..))

-- | Closed type family: is this type @Maybe a@?
type family Nullable (a :: Type) :: Bool where
  Nullable (Maybe _) = 'True
  Nullable _         = 'False

-- | Decode a single column value, handling NULL appropriately.
--
-- For non-Maybe types, NULL produces an error.
-- For @Maybe a@, NULL produces @Nothing@.
--
-- This uses a closed type family ('Nullable') to dispatch without
-- overlapping instances.
class DecodeColumn a where
  decodeColumn :: Vector (Maybe ByteString) -> Int -> Either String a

-- | Implementation dispatcher — selects nullable vs non-nullable decoding
-- based on the closed type family 'Nullable'.
instance (Nullable a ~ flag, DecodeColumnImpl flag a) => DecodeColumn a where
  decodeColumn = decodeColumnImpl @flag
  {-# INLINE decodeColumn #-}

-- | Internal class parameterized by the 'Nullable' flag.
-- Not exported — users only see 'DecodeColumn'.
class DecodeColumnImpl (flag :: Bool) a where
  decodeColumnImpl :: Vector (Maybe ByteString) -> Int -> Either String a

-- | Non-nullable: NULL is an error.
instance (PgDecode a) => DecodeColumnImpl 'False a where
  decodeColumnImpl row idx
    | idx >= V.length row = Left $ colOutOfRange idx (V.length row)
    | otherwise = case row V.! idx of
        Nothing -> Left $ "Column " <> show idx <> " is NULL but expected a non-nullable value"
        Just bs -> pgDecode bs
  {-# INLINE decodeColumnImpl #-}

-- | Nullable: NULL becomes @Nothing@, non-NULL becomes @Just a@.
instance (PgDecode a) => DecodeColumnImpl 'True (Maybe a) where
  decodeColumnImpl row idx
    | idx >= V.length row = Left $ colOutOfRange idx (V.length row)
    | otherwise = case row V.! idx of
        Nothing -> Right Nothing
        Just bs -> Just <$> pgDecode bs
  {-# INLINE decodeColumnImpl #-}

colOutOfRange :: Int -> Int -> String
colOutOfRange idx len =
  "Column index " <> show idx <> " out of range (row has " <> show len <> " columns)"
{-# INLINE colOutOfRange #-}

-- | Decode a result row into a Haskell value.
class FromRow a where
  fromRow :: Vector (Maybe ByteString) -> Either String a

-- Unit instance for commands (INSERT/UPDATE/DELETE) that return no rows
instance {-# OVERLAPPING #-} FromRow () where
  fromRow _ = Right ()

-- Single column
instance (DecodeColumn a) => FromRow a where
  fromRow row = decodeColumn row 0

-- Single-value wrapper for scalar queries
instance {-# OVERLAPPING #-} (DecodeColumn a) => FromRow (Only a) where
  fromRow row = Only <$> decodeColumn row 0

-- | Wrapper for single-column results.
newtype Only a = Only {fromOnly :: a}
  deriving stock (Show, Eq)

-- Tuple instances ---------------------------------------------------------
-- These need OVERLAPPING because they also match the single-column
-- FromRow a instance above. This is the only place we use overlap,
-- and it's benign: tuples are always more specific than a bare type variable.

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b) => FromRow (a, b) where
  fromRow row = (,) <$> decodeColumn row 0 <*> decodeColumn row 1

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c) => FromRow (a, b, c) where
  fromRow row = (,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d) => FromRow (a, b, c, d) where
  fromRow row = (,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e) => FromRow (a, b, c, d, e) where
  fromRow row = (,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f) => FromRow (a, b, c, d, e, f) where
  fromRow row = (,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5

-- PgDecode (Maybe a) for the single-column FromRow a path
instance (PgDecode a) => PgDecode (Maybe a) where
  pgDecode bs = Just <$> pgDecode bs
  {-# INLINE pgDecode #-}
