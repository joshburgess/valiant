-- | Binary encode/decode for PostgreSQL enum types.
--
-- PG sends enum values as text (UTF-8 label) in binary format,
-- identical to the text encoding. This module provides the 'PgEnum'
-- type class for Haskell sum types that map to PG enums.
--
-- @
-- data Mood = Happy | Sad | Angry
--   deriving stock (Show, Eq, Bounded, Enum)
--
-- instance PgEnum Mood where
--   pgEnumToText Happy = "happy"
--   pgEnumToText Sad   = "sad"
--   pgEnumToText Angry = "angry"
--   pgEnumFromText "happy" = Right Happy
--   pgEnumFromText "sad"   = Right Sad
--   pgEnumFromText "angry" = Right Angry
--   pgEnumFromText t       = Left ("unknown mood: " <> T.unpack t)
-- @
module Valiant.Binary.Enum
  ( PgEnum (..)
  , pgEncodeEnum
  , pgDecodeEnum
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

-- | Type class for Haskell types that correspond to PostgreSQL enums.
-- PG encodes enums as their text label in both text and binary format.
class PgEnum a where
  pgEnumToText :: a -> Text
  pgEnumFromText :: Text -> Either String a

-- | Encode an enum value to its PG binary representation (UTF-8 label).
pgEncodeEnum :: (PgEnum a) => a -> ByteString
pgEncodeEnum = TE.encodeUtf8 . pgEnumToText
{-# INLINE pgEncodeEnum #-}

-- | Decode a PG binary enum value (UTF-8 label) to a Haskell value.
pgDecodeEnum :: (PgEnum a) => ByteString -> Either String a
pgDecodeEnum = pgEnumFromText . TE.decodeUtf8
{-# INLINE pgDecodeEnum #-}
