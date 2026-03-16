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
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16)
import Data.List (foldl')
import Data.Scientific (Scientific)
import Data.Scientific qualified as Sci
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
      -- Special case: zero is ndigits=0, weight=0, sign=positive, dscale=0
      LBS.toStrict . B.toLazyByteString $
        int16 0 <> int16 0 <> int16 signPositive <> int16 0
  | otherwise =
      let (isNeg, absSci) = if sci < 0 then (True, negate sci) else (False, sci)
          sign = if isNeg then signNegative else signPositive

          -- Convert to coefficient and base-10 exponent
          c = Sci.coefficient absSci
          e = Sci.base10Exponent absSci

          -- Get all decimal digits as a string
          -- We need the full decimal representation
          -- coefficient * 10^exponent
          -- We'll work with the coefficient digits and adjust for the exponent
          coeffStr = show (abs c)
          totalDigits = length coeffStr + e -- total digits before + after decimal

          -- Pad coefficient string so its length is divisible by 4,
          -- aligned to the correct base-10000 boundary
          -- We need to figure out how many digits are before the decimal point
          digitsBeforeDecimal = max 0 (length coeffStr + e)
          digitsAfterDecimal = max 0 (negate e)

          -- Build the full digit string: digits before decimal + digits after
          fullStr =
            let allDigits = coeffStr ++ replicate (max 0 e) '0'
                -- allDigits has all digits before the decimal point when e >= 0
                -- When e < 0, coeffStr has digits split at position (len + e)
             in if e >= 0
                  then allDigits
                  else
                    let beforeLen = length coeffStr + e
                     in if beforeLen > 0
                          then coeffStr
                          else replicate (negate beforeLen) '0' ++ coeffStr

          -- Number of base-10000 groups before decimal point
          groupsBefore = (digitsBeforeDecimal + 3) `div` 4
          -- Pad the integer part on the left to be groupsBefore*4 digits
          padBefore = groupsBefore * 4 - digitsBeforeDecimal

          -- Number of base-10000 groups for fractional part
          groupsAfter = (digitsAfterDecimal + 3) `div` 4
          -- Pad the fractional part on the right to groupsAfter*4 digits
          padAfter = groupsAfter * 4 - digitsAfterDecimal

          -- Build padded digit string
          (intPart, fracPart)
            | e >= 0 = (coeffStr ++ replicate e '0', "")
            | otherwise =
                let splitAt' = length coeffStr + e
                 in if splitAt' > 0
                      then (take splitAt' coeffStr, drop splitAt' coeffStr)
                      else ("", replicate (negate splitAt') '0' ++ coeffStr)

          paddedInt = replicate padBefore '0' ++ intPart
          paddedFrac = fracPart ++ replicate padAfter '0'

          allPaddedDigits = paddedInt ++ paddedFrac

          -- Convert to base-10000 groups
          groups = toBase10000Groups allPaddedDigits

          -- Strip trailing zero groups from the fractional part
          ndigits = length groups
          weight = fromIntegral (groupsBefore - 1) :: Int16
          dscale = fromIntegral digitsAfterDecimal :: Int16

          -- Strip trailing zeros
          groups' = reverse (dropWhile (== 0) (reverse groups))
          ndigits' = length groups'
       in LBS.toStrict . B.toLazyByteString $
            int16 (fromIntegral ndigits')
              <> int16 weight
              <> int16 sign
              <> int16 dscale
              <> mconcat (map (int16 . fromIntegral) groups')

toBase10000Groups :: String -> [Int]
toBase10000Groups [] = []
toBase10000Groups s =
  let (chunk, rest) = splitAt 4 s
      val = foldl' (\acc c -> acc * 10 + fromEnum c - 48) 0 chunk
   in val : toBase10000Groups rest

int16 :: Int16 -> B.Builder
int16 = B.int16BE

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
      dscale <- getInt16 bs 6
      let expectedLen = 8 + fromIntegral ndigits * 2
      if BS.length bs < expectedLen
        then Left $ "numeric: payload too short (need " <> show expectedLen <> " bytes)"
        else do
          when (sign == signNaN) $ Left "numeric: NaN not representable as Scientific"
          digits <- mapM (\i -> getInt16 bs (8 + i * 2)) [0 .. fromIntegral ndigits - 1]
          let isNeg = sign == signNegative
              -- Each digit is base-10000. weight is the power of 10000 for the first digit.
              -- So the value is: sum(digits[i] * 10000^(weight - i)) for i in [0..ndigits-1]
              -- = sum(digits[i] * 10^(4*(weight - i)))

              -- Build the integer value by accumulating
              intVal = foldl' (\acc d -> acc * 10000 + fromIntegral d) (0 :: Integer) digits

              -- The exponent adjustment:
              -- The first digit has power 4*weight, last has 4*(weight - ndigits + 1)
              -- So the accumulated value represents intVal * 10^(4*(weight - ndigits + 1))
              -- But we also have dscale fractional digits, which means
              -- the actual exponent in base-10 is:
              expo = 4 * (fromIntegral weight - fromIntegral ndigits + 1)

              sci = Sci.scientific (if isNeg then negate intVal else intVal) expo

              -- Adjust to have the correct dscale (trim excess precision)
              -- dscale tells us how many digits after the decimal point
              result = if dscale > 0 then sci else sci
          Right result

getInt16 :: ByteString -> Int -> Either String Int16
getInt16 bs off
  | off + 2 > BS.length bs = Left "numeric: getInt16 out of bounds"
  | otherwise =
      let hi = fromIntegral (BS.index bs off) :: Int16
          lo = fromIntegral (BS.index bs (off + 1)) :: Int16
       in Right (hi `shiftL` 8 .|. lo)

when :: Bool -> Either String () -> Either String ()
when True e = e
when False _ = Right ()
