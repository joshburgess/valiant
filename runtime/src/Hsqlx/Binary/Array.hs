{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.Binary.Array
  ( pgEncodeArray
  , pgDecodeArray
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (Oid (..))

-- | Encode a Vector as a PostgreSQL one-dimensional array in binary format.
--
-- PG binary array format:
--   4 bytes: number of dimensions (1 for 1-D)
--   4 bytes: has-null flag (0 or 1)
--   4 bytes: element OID
--   -- per dimension:
--   4 bytes: dimension size (number of elements)
--   4 bytes: lower bound (1 for standard arrays)
--   -- per element:
--   4 bytes: element length (-1 for NULL)
--   N bytes: element data
pgEncodeArray :: (PgEncode a) => Oid -> Vector a -> ByteString
pgEncodeArray (Oid elemOid) elems =
  LBS.toStrict . B.toLazyByteString $
    B.int32BE 1 -- 1 dimension
      <> B.int32BE 0 -- no nulls
      <> B.word32BE elemOid -- element OID
      <> B.int32BE (fromIntegral (V.length elems)) -- dimension size
      <> B.int32BE 1 -- lower bound (1-based)
      <> V.foldl' (\acc a -> acc <> encodeElement a) mempty elems
  where
    encodeElement a =
      let bs = pgEncode a
       in B.int32BE (fromIntegral (BS.length bs)) <> B.byteString bs

-- | Decode a PostgreSQL one-dimensional binary array to a @Vector@.
--
-- Limitations:
--
-- * Only one-dimensional arrays are supported. Multi-dimensional arrays
--   produce an error.
-- * NULL elements are not supported — PostgreSQL arrays containing NULLs
--   will fail with @\"NULL elements not supported in Vector decode\"@.
--   Use @Vector (Maybe a)@ with a custom decoder if you need nullable
--   array elements.
pgDecodeArray :: (PgDecode a) => ByteString -> Either String (Vector a)
pgDecodeArray bs
  | BS.length bs < 12 = Left "array: header too short (need at least 12 bytes)"
  | otherwise = do
      ndim <- decodeInt32At bs 0
      _hasNull <- decodeInt32At bs 4
      _elemOid <- decodeWord32At bs 8
      case ndim of
        0 -> Right V.empty
        1 -> do
          when (BS.length bs < 20) $ Left "array: 1-D header too short (need at least 20 bytes)"
          nelems <- decodeInt32At bs 12
          _lbound <- decodeInt32At bs 16
          decodeElements bs 20 (fromIntegral nelems)
        _ -> Left $ "array: only 1-D arrays supported, got " <> show ndim <> " dimensions"

decodeElements :: (PgDecode a) => ByteString -> Int -> Int -> Either String (Vector a)
decodeElements bs offset count = go offset count []
  where
    go _off 0 acc = Right (V.fromList (reverse acc))
    go off n acc
      | off + 4 > BS.length bs = Left "array: unexpected end of data reading element length"
      | otherwise = do
          elemLen <- decodeInt32At bs off
          if elemLen == -1
            then Left "array: NULL elements not supported in Vector decode"
            else do
              let dataOff = off + 4
                  dataEnd = dataOff + fromIntegral elemLen
              when (dataEnd > BS.length bs) $
                Left "array: unexpected end of data reading element"
              let elemBs = BS.take (fromIntegral elemLen) (BS.drop dataOff bs)
              val <- pgDecode elemBs
              go dataEnd (n - 1) (val : acc)

-- Helpers -------------------------------------------------------------------

decodeInt32At :: ByteString -> Int -> Either String Int32
decodeInt32At bs off
  | off + 4 > BS.length bs = Left "array: unexpected end of data"
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
