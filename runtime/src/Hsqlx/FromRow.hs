{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Decode result rows into Haskell types.
module Hsqlx.FromRow
  ( FromRow (..)
  , DecodeColumn (..)
  , GFromRow (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Hsqlx.Binary.Decode ()
import PgWire.Binary.Types (PgDecode (..))

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
    | otherwise = case V.unsafeIndex row idx of
        Nothing -> Left $ "Column " <> show idx <> " is NULL but expected a non-nullable value"
        Just bs -> pgDecode bs
  {-# INLINE decodeColumnImpl #-}

-- | Nullable: NULL becomes @Nothing@, non-NULL becomes @Just a@.
instance (PgDecode a) => DecodeColumnImpl 'True (Maybe a) where
  decodeColumnImpl row idx
    | idx >= V.length row = Left $ colOutOfRange idx (V.length row)
    | otherwise = case V.unsafeIndex row idx of
        Nothing -> Right Nothing
        Just bs -> Just <$> pgDecode bs
  {-# INLINE decodeColumnImpl #-}

colOutOfRange :: Int -> Int -> String
colOutOfRange idx len =
  "Column index " <> show idx <> " out of range (row has " <> show len <> " columns)"
{-# INLINE colOutOfRange #-}

-- | Decode a result row into a Haskell value.
--
-- For custom record types, derive via @Generic@:
--
-- @
-- data User = User
--   { userId    :: Int32
--   , userName  :: Text
--   , userEmail :: Maybe Text
--   } deriving stock (Generic)
--     deriving anyclass (FromRow)
-- @
--
-- Fields are decoded positionally (column 0 → first field, etc.).
class FromRow a where
  fromRow :: Vector (Maybe ByteString) -> Either String a
  default fromRow :: (Generic a, GFromRow (Rep a)) => Vector (Maybe ByteString) -> Either String a
  fromRow row = to <$> gFromRow row 0

-- | Generic helper for positional row decoding. Internal — not exported.
class GFromRow f where
  gFromRow :: Vector (Maybe ByteString) -> Int -> Either String (f p)
  gFieldCount :: proxy f -> Int

-- Datatype metadata — delegate
instance (GFromRow f) => GFromRow (M1 D c f) where
  gFromRow row idx = M1 <$> gFromRow row idx
  gFieldCount _ = gFieldCount (undefined :: proxy f)
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

-- Constructor metadata — delegate
instance (GFromRow f) => GFromRow (M1 C c f) where
  gFromRow row idx = M1 <$> gFromRow row idx
  gFieldCount _ = gFieldCount (undefined :: proxy f)
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

-- Selector metadata — delegate
instance (GFromRow f) => GFromRow (M1 S c f) where
  gFromRow row idx = M1 <$> gFromRow row idx
  gFieldCount _ = gFieldCount (undefined :: proxy f)
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

-- Leaf field — decode one column at the current index
instance (DecodeColumn a) => GFromRow (K1 R a) where
  gFromRow row idx = K1 <$> decodeColumn row idx
  gFieldCount _ = 1
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

-- Product — decode left fields, then right fields
instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gFromRow row idx = do
    l <- gFromRow row idx
    r <- gFromRow row (idx + gFieldCount (undefined :: proxy f))
    Right (l :*: r)
  gFieldCount _ = gFieldCount (undefined :: proxy f) + gFieldCount (undefined :: proxy g)
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

-- Unit — no fields
instance GFromRow U1 where
  gFromRow _ _ = Right U1
  gFieldCount _ = 0
  {-# INLINE gFromRow #-}
  {-# INLINE gFieldCount #-}

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

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g) => FromRow (a, b, c, d, e, f, g) where
  fromRow row = (,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h) => FromRow (a, b, c, d, e, f, g, h) where
  fromRow row = (,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i) => FromRow (a, b, c, d, e, f, g, h, i) where
  fromRow row = (,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j) => FromRow (a, b, c, d, e, f, g, h, i, j) where
  fromRow row = (,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k) => FromRow (a, b, c, d, e, f, g, h, i, j, k) where
  fromRow row = (,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k, DecodeColumn l) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l) where
  fromRow row = (,,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10 <*> decodeColumn row 11

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k, DecodeColumn l, DecodeColumn m) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  fromRow row = (,,,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10 <*> decodeColumn row 11 <*> decodeColumn row 12

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k, DecodeColumn l, DecodeColumn m, DecodeColumn n) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  fromRow row = (,,,,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10 <*> decodeColumn row 11 <*> decodeColumn row 12 <*> decodeColumn row 13

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k, DecodeColumn l, DecodeColumn m, DecodeColumn n, DecodeColumn o) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  fromRow row = (,,,,,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10 <*> decodeColumn row 11 <*> decodeColumn row 12 <*> decodeColumn row 13 <*> decodeColumn row 14

instance {-# OVERLAPPING #-} (DecodeColumn a, DecodeColumn b, DecodeColumn c, DecodeColumn d, DecodeColumn e, DecodeColumn f, DecodeColumn g, DecodeColumn h, DecodeColumn i, DecodeColumn j, DecodeColumn k, DecodeColumn l, DecodeColumn m, DecodeColumn n, DecodeColumn o, DecodeColumn p) => FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  fromRow row = (,,,,,,,,,,,,,,,) <$> decodeColumn row 0 <*> decodeColumn row 1 <*> decodeColumn row 2 <*> decodeColumn row 3 <*> decodeColumn row 4 <*> decodeColumn row 5 <*> decodeColumn row 6 <*> decodeColumn row 7 <*> decodeColumn row 8 <*> decodeColumn row 9 <*> decodeColumn row 10 <*> decodeColumn row 11 <*> decodeColumn row 12 <*> decodeColumn row 13 <*> decodeColumn row 14 <*> decodeColumn row 15

-- PgDecode (Maybe a) for the single-column FromRow a path
instance (PgDecode a) => PgDecode (Maybe a) where
  pgDecode bs = Just <$> pgDecode bs
  {-# INLINE pgDecode #-}
