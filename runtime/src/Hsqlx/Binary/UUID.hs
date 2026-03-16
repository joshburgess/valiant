{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary encode/decode for PostgreSQL @uuid@ type.
--
-- PG stores UUID as 16 bytes in network byte order, which is the same
-- as the RFC 4122 binary representation that uuid-types uses.
module Hsqlx.Binary.UUID
  () where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Hsqlx.Binary.Types (PgDecode (..), PgEncode (..))
import Hsqlx.Protocol.Oid (oidUuid)

instance PgEncode UUID where
  pgEncode = LBS.toStrict . UUID.toByteString
  {-# INLINE pgEncode #-}
  pgOid _ = oidUuid
  {-# INLINE pgOid #-}

instance PgDecode UUID where
  pgDecode bs
    | BS.length bs /= 16 = Left $ "uuid: expected 16 bytes, got " <> show (BS.length bs)
    | otherwise = case UUID.fromByteString (LBS.fromStrict bs) of
        Nothing -> Left "uuid: invalid bytes"
        Just u -> Right u
  {-# INLINE pgDecode #-}
