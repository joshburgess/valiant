-- | Binary encode/decode for PostgreSQL composite (row) types.
--
-- PG binary format for composite values:
--   4 bytes: number of fields (Int32)
--   per field:
--     4 bytes: field type OID (Word32)
--     4 bytes: field data length (-1 for NULL)
--     N bytes: field data
module Valiant.Binary.Composite
  ( pgEncodeComposite
  , pgDecodeComposite
  , CompositeField (..)
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Word (Word32)

-- | A single field in a composite value.
data CompositeField = CompositeField
  { cfOid :: Word32
  , cfValue :: Maybe ByteString
  -- ^ Nothing for NULL
  }
  deriving stock (Show, Eq)

-- | Encode a list of composite fields to PG binary format.
pgEncodeComposite :: [CompositeField] -> ByteString
pgEncodeComposite fields =
  LBS.toStrict . B.toLazyByteString $
    B.int32BE (fromIntegral (length fields))
      <> foldMap encodeField fields
  where
    encodeField (CompositeField oid Nothing) =
      B.word32BE oid <> B.int32BE (-1)
    encodeField (CompositeField oid (Just bs)) =
      B.word32BE oid
        <> B.int32BE (fromIntegral (BS.length bs))
        <> B.byteString bs

-- | Decode PG binary composite format to a list of fields.
pgDecodeComposite :: ByteString -> Either String [CompositeField]
pgDecodeComposite bs
  | BS.length bs < 4 = Left "composite: header too short"
  | otherwise = do
      nfields <- decodeInt32At bs 0
      decodeFields bs 4 (fromIntegral nfields)

decodeFields :: ByteString -> Int -> Int -> Either String [CompositeField]
decodeFields _ _ 0 = Right []
decodeFields bs off n
  | off + 8 > BS.length bs = Left "composite: unexpected end reading field header"
  | otherwise = do
      oid <- decodeWord32At bs off
      len <- decodeInt32At bs (off + 4)
      if len == -1
        then do
          rest <- decodeFields bs (off + 8) (n - 1)
          Right (CompositeField oid Nothing : rest)
        else do
          let dataOff = off + 8
              dataEnd = dataOff + fromIntegral len
          when (dataEnd > BS.length bs) $
            Left "composite: unexpected end reading field data"
          let val = BS.take (fromIntegral len) (BS.drop dataOff bs)
          rest <- decodeFields bs dataEnd (n - 1)
          Right (CompositeField oid (Just val) : rest)

-- Helpers -------------------------------------------------------------------

decodeInt32At :: ByteString -> Int -> Either String Int32
decodeInt32At bs off
  | off + 4 > BS.length bs = Left "composite: int32 out of bounds"
  | otherwise =
      let b0 = fromIntegral (BS.index bs off) :: Int32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Int32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Int32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)

decodeWord32At :: ByteString -> Int -> Either String Word32
decodeWord32At bs off = fromIntegral <$> decodeInt32At bs off

when :: Bool -> Either String () -> Either String ()
when True e = e
when False _ = Right ()
