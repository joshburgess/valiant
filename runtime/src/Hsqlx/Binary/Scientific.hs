{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary encode/decode for PostgreSQL @numeric@ type.
--
-- PG stores numeric in a base-10000 representation:
--   2 bytes: ndigits (number of base-10000 digits)
--   2 bytes: weight  (exponent of first digit, in base-10000)
--   2 bytes: sign    (0x0000 = positive, 0x4000 = negative, 0xC000 = NaN)
--   2 bytes: dscale  (number of digits after decimal point)
--   ndigits * 2 bytes: base-10000 digits (0..9999 each, big-endian Int16)
module Hsqlx.Binary.Scientific
  () where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
import Data.Word (Word8)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidNumeric)

-- Constants
signPositive, signNegative, signNaN :: Int16
signPositive = 0x0000
signNegative = 0x4000
signNaN = fromIntegral (0xC000 :: Int)

-- Encoding ------------------------------------------------------------------

instance PgEncode Scientific where
  pgEncode s = encodeNumeric s
  pgOid _ = oidNumeric

encodeNumeric :: Scientific -> ByteString
encodeNumeric sci
  | sci == 0 =
      LBS.toStrict . B.toLazyByteString $
        int16 0 <> int16 0 <> int16 signPositive <> int16 0
  | otherwise =
      let (isNeg, absSci) = if sci < 0 then (True, negate sci) else (False, sci)
          !sign = if isNeg then signNegative else signPositive

          c = Sci.coefficient absSci
          e = Sci.base10Exponent absSci

          -- Digit bytes of the absolute coefficient
          !coeffBs = BS8.pack (show (abs c))
          !coeffLen = BS.length coeffBs

          !digitsBeforeDecimal = max 0 (coeffLen + e)
          !digitsAfterDecimal = max 0 (negate e)

          -- Number of base-10000 groups
          !groupsBefore = (digitsBeforeDecimal + 3) `div` 4
          !padBefore = groupsBefore * 4 - digitsBeforeDecimal
          !groupsAfter = (digitsAfterDecimal + 3) `div` 4
          !padAfter = groupsAfter * 4 - digitsAfterDecimal

          -- Build padded digit bytes: [padBefore zeros] [intPart] [fracPart] [padAfter zeros]
          (!intPart, !fracPart)
            | e >= 0 = (coeffBs <> BS.replicate e 0x30, BS.empty) -- 0x30 = '0'
            | otherwise =
                let splitPos = coeffLen + e
                 in if splitPos > 0
                      then (BS.take splitPos coeffBs, BS.drop splitPos coeffBs)
                      else (BS.empty, BS.replicate (negate splitPos) 0x30 <> coeffBs)

          !allDigits = BS.replicate padBefore 0x30 <> intPart <> fracPart <> BS.replicate padAfter 0x30

          -- Convert to base-10000 groups
          !groups = toBase10000Groups allDigits

          -- Strip trailing zero groups
          !groups' = reverse (dropWhile (== 0) (reverse groups))
          !ndigits' = length groups'
          !weight = fromIntegral (groupsBefore - 1) :: Int16
          !dscale = fromIntegral digitsAfterDecimal :: Int16

       in LBS.toStrict . B.toLazyByteString $
            int16 (fromIntegral ndigits')
              <> int16 weight
              <> int16 sign
              <> int16 dscale
              <> mconcat (map (int16 . fromIntegral) groups')

-- | Convert a ByteString of ASCII digit bytes to base-10000 groups.
-- Processes 4 bytes at a time.
toBase10000Groups :: ByteString -> [Int]
toBase10000Groups bs
  | BS.null bs = []
  | otherwise =
      let (!chunk, !rest) = BS.splitAt 4 bs
          !val = BS.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) 0 chunk
       in val : toBase10000Groups rest

int16 :: Int16 -> B.Builder
int16 = B.int16BE
{-# INLINE int16 #-}

-- Decoding ------------------------------------------------------------------

instance PgDecode Scientific where
  pgDecode = decodeNumeric

decodeNumeric :: ByteString -> Either String Scientific
decodeNumeric bs
  | BS.length bs < 8 = Left "numeric: header too short (need 8 bytes)"
  | otherwise = do
      ndigits <- getInt16 bs 0
      weight <- getInt16 bs 2
      sign <- getInt16 bs 4
      _dscale <- getInt16 bs 6
      let expectedLen = 8 + fromIntegral ndigits * 2
      if BS.length bs < expectedLen
        then Left $ "numeric: payload too short (need " <> show expectedLen <> " bytes)"
        else do
          when (sign == signNaN) $ Left "numeric: NaN not representable as Scientific"
          digits <- mapM (\i -> getInt16 bs (8 + i * 2)) [0 .. fromIntegral ndigits - 1]
          let !isNeg = sign == signNegative
              !intVal = foldlStrict (\acc d -> acc * 10000 + fromIntegral d) (0 :: Integer) digits
              !expo = 4 * (fromIntegral weight - fromIntegral ndigits + 1)
              !sci = Sci.scientific (if isNeg then negate intVal else intVal) expo
          Right sci

-- | Strict left fold over a list (avoids importing Data.List just for foldl').
foldlStrict :: (b -> a -> b) -> b -> [a] -> b
foldlStrict _ !z [] = z
foldlStrict f !z (x : xs) = foldlStrict f (f z x) xs
{-# INLINE foldlStrict #-}

getInt16 :: ByteString -> Int -> Either String Int16
getInt16 bs off
  | off + 2 > BS.length bs = Left "numeric: getInt16 out of bounds"
  | otherwise =
      let hi = fromIntegral (BS.index bs off) :: Int16
          lo = fromIntegral (BS.index bs (off + 1)) :: Int16
       in Right (hi `shiftL` 8 .|. lo)
{-# INLINE getInt16 #-}

when :: Bool -> Either String () -> Either String ()
when True e = e
when False _ = Right ()
{-# INLINE when #-}
