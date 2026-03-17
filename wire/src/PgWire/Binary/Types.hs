-- | Type classes for encoding and decoding Haskell values to and from
-- PostgreSQL binary format.
--
-- Instances are provided for common types ('Int32', 'Text', 'Bool',
-- 'UTCTime', etc.) in "Hsqlx.Binary.Encode" and "Hsqlx.Binary.Decode".
-- Custom types can provide their own instances.
module PgWire.Binary.Types
  ( PgEncode (..)
  , PgDecode (..)
  ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import PgWire.Protocol.Oid (Oid)

-- | Encode a Haskell value to PostgreSQL binary format.
class PgEncode a where
  pgEncode :: a -> ByteString
  pgOid :: Proxy a -> Oid
  {-# MINIMAL pgEncode, pgOid #-}

-- | Decode a Haskell value from PostgreSQL binary format.
class PgDecode a where
  pgDecode :: ByteString -> Either String a
  {-# MINIMAL pgDecode #-}
