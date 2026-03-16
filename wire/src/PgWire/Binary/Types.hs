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
