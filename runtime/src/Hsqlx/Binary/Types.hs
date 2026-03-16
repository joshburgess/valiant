module Hsqlx.Binary.Types
  ( PgEncode (..)
  , PgDecode (..)
  ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy)
import Hsqlx.Protocol.Oid (Oid)

-- | Encode a Haskell value to PostgreSQL binary format.
class PgEncode a where
  pgEncode :: a -> ByteString
  pgOid :: Proxy a -> Oid

-- | Decode a Haskell value from PostgreSQL binary format.
class PgDecode a where
  pgDecode :: ByteString -> Either String a
