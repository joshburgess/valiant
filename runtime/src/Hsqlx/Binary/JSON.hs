{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary encode/decode for PostgreSQL @json@ and @jsonb@ types.
--
-- JSON binary format: raw UTF-8 JSON text (same as text format).
-- JSONB binary format: version byte (0x01) followed by UTF-8 JSON text.
--
-- We provide instances for @aeson@'s 'Value' type. For json columns,
-- encoding is just the JSON bytes. For jsonb, PG prepends a version byte
-- on the wire, but the extended query protocol handles this transparently
-- when using binary format — the version byte is part of the binary
-- representation that PG sends/expects.
module Hsqlx.Binary.JSON
  () where

import Data.Aeson (Value, eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Hsqlx.Binary.Types (PgDecode (..), PgEncode (..))
import Hsqlx.Protocol.Oid (oidJson)

-- | Encode a JSON Value. Works for both json and jsonb columns.
-- For jsonb, PG expects a version byte (0x01) prefix in binary format.
instance PgEncode Value where
  pgEncode = LBS.toStrict . encode
  {-# INLINE pgEncode #-}
  pgOid _ = oidJson
  {-# INLINE pgOid #-}

-- | Decode a JSON Value. Handles both json (raw JSON) and jsonb
-- (version byte + JSON) binary formats.
instance PgDecode Value where
  pgDecode bs
    -- jsonb binary format: first byte is version (0x01), rest is JSON
    | not (BS.null bs) && BS.index bs 0 == 1 =
        case eitherDecodeStrict' (BS.drop 1 bs) of
          Left err -> Left ("jsonb decode: " <> err)
          Right v -> Right v
    -- json binary format: raw JSON bytes
    | otherwise =
        case eitherDecodeStrict' bs of
          Left err -> Left ("json decode: " <> err)
          Right v -> Right v
  {-# INLINE pgDecode #-}
