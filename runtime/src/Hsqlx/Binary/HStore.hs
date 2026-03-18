{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary codec for the PostgreSQL @hstore@ extension type.
--
-- PG binary format for hstore:
--   4 bytes: number of pairs (Int32)
--   per pair:
--     4 bytes: key length (Int32)
--     N bytes: key data (UTF-8)
--     4 bytes: value length (Int32, -1 for NULL)
--     N bytes: value data (UTF-8)
--
-- The extension must be enabled: @CREATE EXTENSION IF NOT EXISTS hstore@.
-- hstore OID is not fixed — it depends on installation. The OID is
-- typically resolved dynamically via @pg_type@ during @hsqlx prepare@.
module Hsqlx.Binary.HStore
  ( PgHStore (..)
  , hstoreFromList
  , hstoreToList
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Bits (shiftL, (.|.))
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (Oid (..))

-- | A PostgreSQL @hstore@ value — a set of key/value string pairs.
-- Values are nullable (a key can map to SQL NULL).
newtype PgHStore = PgHStore
  { unHStore :: Map Text (Maybe Text)
  -- ^ The underlying map. 'Nothing' values represent SQL NULL.
  }
  deriving stock (Show, Eq, Generic)

instance NFData PgHStore

-- | Construct a 'PgHStore' from a list of key/value pairs.
-- All values are non-NULL.
--
-- @
-- hstoreFromList [(\"color\", \"red\"), (\"size\", \"large\")]
-- @
hstoreFromList :: [(Text, Text)] -> PgHStore
hstoreFromList = PgHStore . Map.fromList . map (\(k, v) -> (k, Just v))

-- | Extract non-NULL key/value pairs as a list.
hstoreToList :: PgHStore -> [(Text, Text)]
hstoreToList (PgHStore m) =
  [(k, v) | (k, Just v) <- Map.toList m]

-- | hstore extension OID is installation-dependent. We use 0 here as a
-- placeholder; the actual OID is resolved at prepare time.
oidHStore :: Oid
oidHStore = Oid 0

instance PgEncode PgHStore where
  pgEncode (PgHStore m) =
    let pairs = Map.toList m
        nPairs = fromIntegral (length pairs) :: Int32
     in LBS.toStrict . B.toLazyByteString $
          B.int32BE nPairs <> foldMap encodePair pairs
    where
      encodePair (k, mv) =
        let kbs = T.encodeUtf8 k
         in B.int32BE (fromIntegral (BS.length kbs))
              <> B.byteString kbs
              <> case mv of
                   Nothing -> B.int32BE (-1)
                   Just v ->
                     let vbs = T.encodeUtf8 v
                      in B.int32BE (fromIntegral (BS.length vbs))
                           <> B.byteString vbs
  {-# INLINE pgEncode #-}
  pgOid _ = oidHStore
  {-# INLINE pgOid #-}

instance PgDecode PgHStore where
  pgDecode bs
    | BS.length bs < 4 = Left "hstore: too short for pair count"
    | otherwise = do
        nPairs <- decodeInt32At bs 0
        decodePairs bs 4 (fromIntegral nPairs :: Int) []
    where
      decodePairs _ _ 0 !acc = Right (PgHStore (Map.fromList (reverse acc)))
      decodePairs b off n !acc
        | off + 4 > BS.length b = Left "hstore: unexpected end reading key length"
        | otherwise = do
            kLen <- decodeInt32At b off
            let kOff = off + 4
                kEnd = kOff + fromIntegral kLen
            if kEnd > BS.length b
              then Left "hstore: unexpected end reading key data"
              else do
                let key = T.decodeUtf8 (BS.take (fromIntegral kLen) (BS.drop kOff b))
                if kEnd + 4 > BS.length b
                  then Left "hstore: unexpected end reading value length"
                  else do
                    vLen <- decodeInt32At b kEnd
                    if vLen == -1
                      then decodePairs b (kEnd + 4) (n - 1) ((key, Nothing) : acc)
                      else do
                        let vOff = kEnd + 4
                            vEnd = vOff + fromIntegral vLen
                        if vEnd > BS.length b
                          then Left "hstore: unexpected end reading value data"
                          else do
                            let val = T.decodeUtf8 (BS.take (fromIntegral vLen) (BS.drop vOff b))
                            decodePairs b vEnd (n - 1) ((key, Just val) : acc)
  {-# INLINE pgDecode #-}

decodeInt32At :: ByteString -> Int -> Either String Int32
decodeInt32At bs off
  | off + 4 > BS.length bs = Left "hstore: int32 out of bounds"
  | otherwise =
      let b0 = fromIntegral (BS.index bs off) :: Int32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Int32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Int32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
