{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary codec for PostgreSQL @inet@ and @cidr@ types.
--
-- PG binary format (both inet and cidr):
--
--   1 byte: address family (2 = IPv4, 3 = IPv6)
--   1 byte: prefix length (netmask bits: 0–32 for v4, 0–128 for v6)
--   1 byte: is_cidr flag (0 = inet, 1 = cidr)
--   1 byte: address length (4 for IPv4, 16 for IPv6)
--   N bytes: address bytes (network byte order)
--
-- Total: 8 bytes for IPv4, 20 bytes for IPv6.
module Valiant.Binary.Inet
  ( PgInet (..)
  , ipv4
  , ipv4Host
  , ipv6
  , ipv6Host
  , inetToText
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import Numeric (showHex)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidInet)

-- | A PostgreSQL @inet@ or @cidr@ value.
--
-- Represents an IPv4 or IPv6 address with a prefix length (CIDR mask).
-- The 'inetIsCidr' flag distinguishes between @inet@ (host + optional mask)
-- and @cidr@ (network address, host bits must be zero).
data PgInet = PgInet
  { inetAddress :: !ByteString
  -- ^ Raw address bytes: 4 bytes for IPv4, 16 bytes for IPv6.
  , inetPrefix :: {-# UNPACK #-} !Word8
  -- ^ Prefix length (0–32 for IPv4, 0–128 for IPv6).
  , inetIsCidr :: !Bool
  -- ^ 'True' for @cidr@, 'False' for @inet@.
  }
  deriving stock (Eq, Ord, Generic)

instance NFData PgInet

-- | Renders in CIDR notation: @\"192.168.1.0/24\"@ or @\"::1/128\"@.
instance Show PgInet where
  show = T.unpack . inetToText

-- | Construct an IPv4 @inet@ value with a prefix length.
--
-- @ipv4 192 168 1 0 24@ represents @192.168.1.0\/24@.
ipv4 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> PgInet
ipv4 a b c d prefix =
  PgInet
    { inetAddress = BS.pack [a, b, c, d]
    , inetPrefix = prefix
    , inetIsCidr = False
    }

-- | Construct an IPv4 @inet@ host (prefix = 32).
ipv4Host :: Word8 -> Word8 -> Word8 -> Word8 -> PgInet
ipv4Host a b c d = ipv4 a b c d 32

-- | Construct an IPv6 @inet@ value from raw address bytes with a prefix
-- length. The 'ByteString' must be exactly 16 bytes.
ipv6 :: ByteString -> Word8 -> PgInet
ipv6 addr prefix =
  PgInet
    { inetAddress = addr
    , inetPrefix = prefix
    , inetIsCidr = False
    }

-- | Construct an IPv6 @inet@ host (prefix = 128) from raw address bytes.
-- The 'ByteString' must be exactly 16 bytes.
ipv6Host :: ByteString -> PgInet
ipv6Host addr = ipv6 addr 128

-- | Render a 'PgInet' as CIDR notation text.
--
-- >>> inetToText (ipv4 192 168 1 0 24)
-- "192.168.1.0/24"
-- >>> inetToText (ipv6Host (BS.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]))
-- "::1/128"
inetToText :: PgInet -> Text
inetToText (PgInet addr prefix _isCidr)
  | BS.length addr == 4 = renderIPv4 addr <> "/" <> T.pack (show prefix)
  | BS.length addr == 16 = renderIPv6 addr <> "/" <> T.pack (show prefix)
  | otherwise = "<invalid inet>"

renderIPv4 :: ByteString -> Text
renderIPv4 bs =
  T.pack $
    intercalate "." $
      map (\i -> show (BS.index bs i)) [0 .. 3]

renderIPv6 :: ByteString -> Text
renderIPv6 bs =
  let groups :: [Word16]
      groups =
        [ fromIntegral (BS.index bs i) * 256 + fromIntegral (BS.index bs (i + 1))
        | i <- [0, 2 .. 14]
        ]
      hexGroups = map (\g -> T.pack (showHex g "")) groups
   in compressIPv6 hexGroups

-- | Compress an IPv6 address by replacing the longest run of zero groups
-- with "::".
compressIPv6 :: [Text] -> Text
compressIPv6 groups =
  let isZero g = g == "0"
      -- Find runs of zeros
      indexed = zip [0 :: Int ..] (map isZero groups)
      runs = findZeroRuns indexed
   in case runs of
        [] -> T.intercalate ":" groups
        _ ->
          let (start, len) = longestRun runs
              before = take start groups
              after = drop (start + len) groups
           in case (null before, null after) of
                (True, True) -> "::"
                (True, False) -> "::" <> T.intercalate ":" after
                (False, True) -> T.intercalate ":" before <> "::"
                (False, False) -> T.intercalate ":" before <> "::" <> T.intercalate ":" after

findZeroRuns :: [(Int, Bool)] -> [(Int, Int)]
findZeroRuns [] = []
findZeroRuns ((i, True) : rest) =
  let runLen = 1 + length (takeWhile snd rest)
      remaining = drop (runLen - 1) rest
   in (i, runLen) : findZeroRuns remaining
findZeroRuns (_ : rest) = findZeroRuns rest

longestRun :: [(Int, Int)] -> (Int, Int)
longestRun = foldl1 (\a@(_, la) b@(_, lb) -> if lb > la then b else a)

-- Codec -------------------------------------------------------------------

-- PG address family codes
pgAfInet :: Word8
pgAfInet = 2

pgAfInet6 :: Word8
pgAfInet6 = 3

instance PgEncode PgInet where
  pgEncode (PgInet addr prefix isCidr) =
    let addrLen = BS.length addr
        family
          | addrLen == 4 = pgAfInet
          | otherwise = pgAfInet6
        cidrFlag :: Word8
        cidrFlag = if isCidr then 1 else 0
     in LBS.toStrict . B.toLazyByteString $
          B.word8 family
            <> B.word8 prefix
            <> B.word8 cidrFlag
            <> B.word8 (fromIntegral addrLen)
            <> B.byteString addr
  {-# INLINE pgEncode #-}
  pgOid _ = oidInet
  {-# INLINE pgOid #-}

instance PgDecode PgInet where
  pgDecode bs
    | BS.length bs < 4 = Left $ "inet: expected at least 4 header bytes, got " <> show (BS.length bs)
    | otherwise =
        let family = BS.index bs 0
            prefix = BS.index bs 1
            cidrFlag = BS.index bs 2
            addrLen = fromIntegral (BS.index bs 3) :: Int
            expectedFamily
              | addrLen == 4 = pgAfInet
              | addrLen == 16 = pgAfInet6
              | otherwise = 0
         in if BS.length bs /= 4 + addrLen
              then Left $ "inet: expected " <> show (4 + addrLen) <> " bytes, got " <> show (BS.length bs)
              else
                if family /= expectedFamily
                  then Left $ "inet: address family mismatch: " <> show family <> " for " <> show addrLen <> " byte address"
                  else
                    Right
                      PgInet
                        { inetAddress = BS.drop 4 bs
                        , inetPrefix = prefix
                        , inetIsCidr = cidrFlag /= 0
                        }
  {-# INLINE pgDecode #-}
