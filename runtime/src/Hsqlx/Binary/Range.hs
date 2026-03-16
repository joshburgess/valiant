-- | Binary encode/decode for PostgreSQL range types.
--
-- PG binary format for ranges:
--   1 byte: flags
--     0x01 = empty
--     0x02 = has lower bound
--     0x04 = has upper bound
--     0x08 = lower bound inclusive
--     0x10 = upper bound inclusive
--   if has lower bound:
--     4 bytes: lower bound data length
--     N bytes: lower bound data
--   if has upper bound:
--     4 bytes: upper bound data length
--     N bytes: upper bound data
module Hsqlx.Binary.Range
  ( PgRange (..)
  , RangeBound (..)
  , pgEncodeRange
  , pgDecodeRange
  ) where

import Data.Bits (shiftL, testBit, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Word (Word8)

-- | A bound of a range.
data RangeBound a
  = Inclusive a
  | Exclusive a
  deriving stock (Show, Eq, Ord)

-- | A PostgreSQL range value.
data PgRange a
  = EmptyRange
  | PgRange (Maybe (RangeBound a)) (Maybe (RangeBound a))
  -- ^ Lower bound, upper bound. Nothing means unbounded.
  deriving stock (Show, Eq)

-- Flags
flagEmpty, flagHasLower, flagHasUpper, flagLowerInc, flagUpperInc :: Word8
flagEmpty = 0x01
flagHasLower = 0x02
flagHasUpper = 0x04
flagLowerInc = 0x08
flagUpperInc = 0x10

-- | Encode a range value given an element encoder.
pgEncodeRange :: (a -> ByteString) -> PgRange a -> ByteString
pgEncodeRange _ EmptyRange =
  BS.singleton flagEmpty
pgEncodeRange enc (PgRange mLower mUpper) =
  LBS.toStrict . B.toLazyByteString $
    B.word8 flags
      <> encodeBound enc mLower
      <> encodeBound enc mUpper
  where
    flags =
      (if hasBound mLower then flagHasLower else 0)
        .|. (if hasBound mUpper then flagHasUpper else 0)
        .|. (if isInclusive mLower then flagLowerInc else 0)
        .|. (if isInclusive mUpper then flagUpperInc else 0)

    hasBound Nothing = False
    hasBound (Just _) = True

    isInclusive (Just (Inclusive _)) = True
    isInclusive _ = False

encodeBound :: (a -> ByteString) -> Maybe (RangeBound a) -> B.Builder
encodeBound _ Nothing = mempty
encodeBound enc (Just bound) =
  let bs = enc (boundValue bound)
   in B.int32BE (fromIntegral (BS.length bs)) <> B.byteString bs

boundValue :: RangeBound a -> a
boundValue (Inclusive a) = a
boundValue (Exclusive a) = a

-- | Decode a range value given an element decoder.
pgDecodeRange :: (ByteString -> Either String a) -> ByteString -> Either String (PgRange a)
pgDecodeRange _ bs
  | BS.null bs = Left "range: empty input"
pgDecodeRange dec bs = do
  let flags = BS.index bs 0
  if testBit flags 0
    then Right EmptyRange
    else do
      let hasLower = testBit flags 1
          hasUpper = testBit flags 2
          lowerInc = testBit flags 3
          upperInc = testBit flags 4
      (mLower, off1) <-
        if hasLower
          then do
            (val, off) <- decodeBoundAt dec bs 1
            let bound = if lowerInc then Inclusive val else Exclusive val
            Right (Just bound, off)
          else Right (Nothing, 1)
      (mUpper, _) <-
        if hasUpper
          then do
            (val, off) <- decodeBoundAt dec bs off1
            let bound = if upperInc then Inclusive val else Exclusive val
            Right (Just bound, off)
          else Right (Nothing, off1)
      Right (PgRange mLower mUpper)

decodeBoundAt :: (ByteString -> Either String a) -> ByteString -> Int -> Either String (a, Int)
decodeBoundAt dec bs off
  | off + 4 > BS.length bs = Left "range: unexpected end reading bound length"
  | otherwise = do
      len <- decodeInt32At bs off
      let dataOff = off + 4
          dataEnd = dataOff + fromIntegral len
      if dataEnd > BS.length bs
        then Left "range: unexpected end reading bound data"
        else do
          let val = BS.take (fromIntegral len) (BS.drop dataOff bs)
          a <- dec val
          Right (a, dataEnd)

decodeInt32At :: ByteString -> Int -> Either String Int32
decodeInt32At bs off
  | off + 4 > BS.length bs = Left "range: int32 out of bounds"
  | otherwise =
      let b0 = fromIntegral (BS.index bs off) :: Int32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Int32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Int32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
