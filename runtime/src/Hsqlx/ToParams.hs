{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.ToParams
  ( ToParams (..)
  , EncodeField (..)
  ) where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
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
