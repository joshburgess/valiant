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
import Hsqlx.Binary.Types (PgEncode (..))

-- | Encode a single field, handling @Maybe@ for nullable parameters.
class EncodeField a where
  encodeField :: a -> Maybe ByteString

instance {-# OVERLAPPABLE #-} (PgEncode a) => EncodeField a where
  encodeField = Just . pgEncode

instance (PgEncode a) => EncodeField (Maybe a) where
  encodeField Nothing = Nothing
  encodeField (Just a) = Just (pgEncode a)

-- | Encode query parameters into a vector of nullable byte strings.
class ToParams a where
  toParams :: a -> Vector (Maybe ByteString)

instance ToParams () where
  toParams () = V.empty

instance {-# OVERLAPPABLE #-} (EncodeField a) => ToParams a where
  toParams a = V.singleton (encodeField a)

instance (EncodeField a, EncodeField b) => ToParams (a, b) where
  toParams (a, b) = V.fromList [encodeField a, encodeField b]

instance (EncodeField a, EncodeField b, EncodeField c) => ToParams (a, b, c) where
  toParams (a, b, c) = V.fromList [encodeField a, encodeField b, encodeField c]

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d) => ToParams (a, b, c, d) where
  toParams (a, b, c, d) = V.fromList [encodeField a, encodeField b, encodeField c, encodeField d]

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e) => ToParams (a, b, c, d, e) where
  toParams (a, b, c, d, e) = V.fromList [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e]

instance (EncodeField a, EncodeField b, EncodeField c, EncodeField d, EncodeField e, EncodeField f) => ToParams (a, b, c, d, e, f) where
  toParams (a, b, c, d, e, f) = V.fromList [encodeField a, encodeField b, encodeField c, encodeField d, encodeField e, encodeField f]
