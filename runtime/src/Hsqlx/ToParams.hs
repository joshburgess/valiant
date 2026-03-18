{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Encode Haskell values into PostgreSQL binary parameter format.
--
-- 'ToParams' encodes query parameter tuples, while 'EncodeField' handles
-- individual fields. @Maybe a@ encodes as SQL NULL when @Nothing@.
-- Instances are provided for @()@, single values, tuples up to 10,
-- and any @Generic@ type via @DefaultSignatures@.
--
-- For custom record types, derive via @Generic@:
--
-- @
-- data InsertParams = InsertParams
--   { ipName  :: Text
--   , ipEmail :: Maybe Text
--   } deriving stock (Generic)
--     deriving anyclass (ToParams)
-- @
--
-- Fields are encoded positionally (first field → $1, etc.).
module Hsqlx.ToParams
  ( ToParams (..)
  , EncodeField (..)
  ) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Hsqlx.Binary.Encode ()
import PgWire.Binary.Types (PgEncode (..))

-- | Encode a single field, handling @Maybe@ for nullable parameters.
class EncodeField a where
  encodeField :: a -> Maybe ByteString

instance {-# OVERLAPPABLE #-} (PgEncode a) => EncodeField a where
  encodeField = Just . pgEncode
  {-# INLINE encodeField #-}

instance (PgEncode a) => EncodeField (Maybe a) where
  encodeField Nothing = Nothing
  encodeField (Just a) = Just (pgEncode a)
  {-# INLINE encodeField #-}

-- | Encode query parameters into a vector of nullable byte strings.
class ToParams a where
  toParams :: a -> Vector (Maybe ByteString)
  default toParams :: (Generic a, GToParams (Rep a)) => a -> Vector (Maybe ByteString)
  toParams = gToParams . from

-- | Generic helper for positional parameter encoding. Internal — not exported.
class GToParams f where
  gToParams :: f p -> Vector (Maybe ByteString)

-- Datatype metadata — delegate
instance (GToParams f) => GToParams (M1 D c f) where
  gToParams (M1 x) = gToParams x
  {-# INLINE gToParams #-}

-- Constructor metadata — delegate
instance (GToParams f) => GToParams (M1 C c f) where
  gToParams (M1 x) = gToParams x
  {-# INLINE gToParams #-}

-- Selector metadata — delegate
instance (GToParams f) => GToParams (M1 S c f) where
  gToParams (M1 x) = gToParams x
  {-# INLINE gToParams #-}

-- Leaf field — encode one field
instance (EncodeField a) => GToParams (K1 R a) where
  gToParams (K1 a) = V.singleton (encodeField a)
  {-# INLINE gToParams #-}

-- Product — concatenate left and right.
-- V.++ is O(n) per call, but with INLINE GHC fuses the chain for small records.
-- For records with > ~20 fields, a DList-based approach would be better.
instance (GToParams f, GToParams g) => GToParams (f :*: g) where
  gToParams (l :*: r) = gToParams l V.++ gToParams r
  {-# INLINE gToParams #-}

-- Unit — no fields
instance GToParams U1 where
  gToParams U1 = V.empty
  {-# INLINE gToParams #-}

instance ToParams () where
  toParams () = V.empty

instance {-# OVERLAPPABLE #-} (EncodeField a) => ToParams a where
  toParams a = V.singleton (encodeField a)

instance (EncodeField a, EncodeField b) => ToParams (a, b) where
  toParams (a, b) = V.fromListN 2 [encodeField a, encodeField b]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c) => ToParams (a, b, c) where
  toParams (a, b, c) = V.fromListN 3 [encodeField a, encodeField b, encodeField c]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d) => ToParams (a, b, c, d) where
  toParams (a, b, c, d) = V.fromListN 4 [encodeField a, encodeField b, encodeField c, encodeField d]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e) => ToParams (a, b, c, d, e) where
  toParams (a, b, c, d, e) = V.fromListN 5 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f) => ToParams (a, b, c, d, e, f) where
  toParams (a, b, c, d, e, f) = V.fromListN 6 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g) => ToParams (a, b, c, d, e, f, g) where
  toParams (a, b, c, d, e, f, g) = V.fromListN 7 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h) => ToParams (a, b, c, d, e, f, g, h) where
  toParams (a, b, c, d, e, f, g, h) = V.fromListN 8 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i) => ToParams (a, b, c, d, e, f, g, h, i) where
  toParams (a, b, c, d, e, f, g, h, i) = V.fromListN 9 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j) => ToParams (a, b, c, d, e, f, g, h, i, j) where
  toParams (a, b, c, d, e, f, g, h, i, j) = V.fromListN 10 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k) => ToParams (a, b, c, d, e, f, g, h, i, j, k) where
  toParams (a, b, c, d, e, f, g, h, i, j, k) = V.fromListN 11 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k, EncodeField l) => ToParams (a, b, c, d, e, f, g, h, i, j, k, l) where
  toParams (a, b, c, d, e, f, g, h, i, j, k, l) = V.fromListN 12 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k, encodeField l]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k, EncodeField l, EncodeField m) => ToParams (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  toParams (a, b, c, d, e, f, g, h, i, j, k, l, m) = V.fromListN 13 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k, encodeField l, encodeField m]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k, EncodeField l, EncodeField m, EncodeField n) => ToParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  toParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = V.fromListN 14 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k, encodeField l, encodeField m, encodeField n]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k, EncodeField l, EncodeField m, EncodeField n, EncodeField o) => ToParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  toParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = V.fromListN 15 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k, encodeField l, encodeField m, encodeField n, encodeField o]
  {-# INLINE toParams #-}

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f, EncodeField g, EncodeField h, EncodeField i, EncodeField j, EncodeField k, EncodeField l, EncodeField m, EncodeField n, EncodeField o, EncodeField p) => ToParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  toParams (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = V.fromListN 16 [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f, encodeField g, encodeField h, encodeField i, encodeField j, encodeField k, encodeField l, encodeField m, encodeField n, encodeField o, encodeField p]
  {-# INLINE toParams #-}
