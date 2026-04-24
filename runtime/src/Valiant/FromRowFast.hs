{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Fast row decoding that throws exceptions instead of returning Either.
--
-- 'FromRowFast' eliminates the per-column @Right@ wrapper allocation
-- that 'FromRow' requires. On wide rows (10+ columns), this can be
-- 10-20% faster because:
--
-- 1. No @Either String a@ allocation per column decode
-- 2. No @<*>@ chain threading the @Either@ through Applicative
-- 3. Direct constructor application without wrapping
--
-- The trade-off: decode errors become imprecise exceptions instead of
-- structured @Left@ values. Since decode errors indicate a bug (schema
-- mismatch), not a runtime condition, this is acceptable for most code.
--
-- @
-- -- Use fetchAllFast instead of fetchAll for hot paths:
-- users <- fetchAllFast conn listAllUsers ()
-- @
module Valiant.FromRowFast
  ( FromRowFast (..)
  , DecodeColumnFast (..)
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Valiant.Binary.Decode ()
import PgWire.Binary.Types (PgDecode (..))

-- | Closed type family: is this type @Maybe a@?
type family Nullable (a :: Type) :: Bool where
  Nullable (Maybe _) = 'True
  Nullable _         = 'False

-- | Decode a single column, throwing on errors instead of returning Either.
-- No bounds checking — the DataRow parser already validated column count.
class DecodeColumnFast a where
  decodeColumnFast :: Vector (Maybe ByteString) -> Int -> a

instance (Nullable a ~ flag, DecodeColumnFastImpl flag a) => DecodeColumnFast a where
  decodeColumnFast = decodeColumnFastImpl @flag
  {-# INLINE decodeColumnFast #-}

class DecodeColumnFastImpl (flag :: Bool) a where
  decodeColumnFastImpl :: Vector (Maybe ByteString) -> Int -> a

-- Non-nullable: NULL throws, decode errors throw.
instance (PgDecode a) => DecodeColumnFastImpl 'False a where
  decodeColumnFastImpl row idx =
    case V.unsafeIndex row idx of
      Nothing -> error $ "Column " <> show idx <> " is NULL"
      Just bs -> case pgDecode bs of
        Left err -> error err
        Right !val -> val
  {-# INLINE decodeColumnFastImpl #-}

-- Nullable: NULL → Nothing, decode errors throw.
instance (PgDecode a) => DecodeColumnFastImpl 'True (Maybe a) where
  decodeColumnFastImpl row idx =
    case V.unsafeIndex row idx of
      Nothing -> Nothing
      Just bs -> case pgDecode bs of
        Left err -> error err
        Right !val -> Just val
  {-# INLINE decodeColumnFastImpl #-}

-- | Fast row decoder. Throws on errors instead of returning Either.
--
-- Derive via @Generic@:
--
-- @
-- data User = User
--   { userId :: Int32, userName :: Text, userEmail :: Maybe Text }
--   deriving stock (Generic)
--   deriving anyclass (FromRowFast)
-- @
class FromRowFast a where
  fromRowFast :: Vector (Maybe ByteString) -> a
  default fromRowFast :: (Generic a, GFromRowFast (Rep a)) => Vector (Maybe ByteString) -> a
  fromRowFast row = to (gFromRowFast row 0)

-- Generic machinery (same structure as FromRow but without Either)
class GFromRowFast f where
  gFromRowFast :: Vector (Maybe ByteString) -> Int -> f p
  gFieldCountFast :: proxy f -> Int

instance (GFromRowFast f) => GFromRowFast (M1 D c f) where
  gFromRowFast row idx = M1 (gFromRowFast row idx)
  gFieldCountFast _ = gFieldCountFast (undefined :: proxy f)
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

instance (GFromRowFast f) => GFromRowFast (M1 C c f) where
  gFromRowFast row idx = M1 (gFromRowFast row idx)
  gFieldCountFast _ = gFieldCountFast (undefined :: proxy f)
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

instance (GFromRowFast f) => GFromRowFast (M1 S c f) where
  gFromRowFast row idx = M1 (gFromRowFast row idx)
  gFieldCountFast _ = gFieldCountFast (undefined :: proxy f)
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

instance (DecodeColumnFast a) => GFromRowFast (K1 R a) where
  gFromRowFast row idx = K1 (decodeColumnFast row idx)
  gFieldCountFast _ = 1
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

instance (GFromRowFast f, GFromRowFast g) => GFromRowFast (f :*: g) where
  gFromRowFast row idx =
    let !l = gFromRowFast row idx
        !r = gFromRowFast row (idx + gFieldCountFast (undefined :: proxy f))
     in l :*: r
  gFieldCountFast _ = gFieldCountFast (undefined :: proxy f) + gFieldCountFast (undefined :: proxy g)
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

instance GFromRowFast U1 where
  gFromRowFast _ _ = U1
  gFieldCountFast _ = 0
  {-# INLINE gFromRowFast #-}
  {-# INLINE gFieldCountFast #-}

-- ── Instances ──────────────────────────────────────────────────────

instance {-# OVERLAPPING #-} FromRowFast () where
  fromRowFast _ = ()

instance (DecodeColumnFast a) => FromRowFast a where
  fromRowFast row = decodeColumnFast row 0

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b) => FromRowFast (a, b) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c) => FromRowFast (a, b, c) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d) => FromRowFast (a, b, c, d) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e) => FromRowFast (a, b, c, d, e) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f) => FromRowFast (a, b, c, d, e, f) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g) => FromRowFast (a, b, c, d, e, f, g) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g, DecodeColumnFast h) => FromRowFast (a, b, c, d, e, f, g, h) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6, decodeColumnFast row 7)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g, DecodeColumnFast h, DecodeColumnFast i) => FromRowFast (a, b, c, d, e, f, g, h, i) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6, decodeColumnFast row 7, decodeColumnFast row 8)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g, DecodeColumnFast h, DecodeColumnFast i, DecodeColumnFast j) => FromRowFast (a, b, c, d, e, f, g, h, i, j) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6, decodeColumnFast row 7, decodeColumnFast row 8, decodeColumnFast row 9)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g, DecodeColumnFast h, DecodeColumnFast i, DecodeColumnFast j, DecodeColumnFast k) => FromRowFast (a, b, c, d, e, f, g, h, i, j, k) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6, decodeColumnFast row 7, decodeColumnFast row 8, decodeColumnFast row 9, decodeColumnFast row 10)

instance {-# OVERLAPPING #-} (DecodeColumnFast a, DecodeColumnFast b, DecodeColumnFast c, DecodeColumnFast d, DecodeColumnFast e, DecodeColumnFast f, DecodeColumnFast g, DecodeColumnFast h, DecodeColumnFast i, DecodeColumnFast j, DecodeColumnFast k, DecodeColumnFast l) => FromRowFast (a, b, c, d, e, f, g, h, i, j, k, l) where
  fromRowFast row = (decodeColumnFast row 0, decodeColumnFast row 1, decodeColumnFast row 2, decodeColumnFast row 3, decodeColumnFast row 4, decodeColumnFast row 5, decodeColumnFast row 6, decodeColumnFast row 7, decodeColumnFast row 8, decodeColumnFast row 9, decodeColumnFast row 10, decodeColumnFast row 11)
