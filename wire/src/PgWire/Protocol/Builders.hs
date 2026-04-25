-- | Encoders for frontend (client-to-server) messages.
--
-- __Audience:__ exposed for downstream library authors that need to
-- emit raw protocol bytes (e.g. custom pipelining or proxies). Most
-- application code drives these encoders indirectly through
-- "PgWire.Connection".
module PgWire.Protocol.Builders
  ( buildFrontendMsg
  , buildFrontendMsgsConcat
  , buildStartup
  ) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32)
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Protocol.Frontend

-- | Build a complete frontend message as a strict 'ByteString'.
buildFrontendMsg :: FrontendMsg -> ByteString
buildFrontendMsg = LBS.toStrict . B.toLazyByteString . encodeFrontendMsg
{-# INLINE buildFrontendMsg #-}

-- | Fuse multiple frontend messages into a single strict 'ByteString'.
-- Uses one 'Builder' pass and one 'toStrict' call, eliminating N-1
-- intermediate allocations compared to mapping 'buildFrontendMsg'.
buildFrontendMsgsConcat :: [FrontendMsg] -> ByteString
buildFrontendMsgsConcat = LBS.toStrict . B.toLazyByteString . foldMap encodeFrontendMsg
{-# INLINE buildFrontendMsgsConcat #-}

-- | Build the startup message (special: no tag byte).
buildStartup :: StartupParams -> ByteString
buildStartup = LBS.toStrict . B.toLazyByteString . encodeStartup

-- Encoding ----------------------------------------------------------------
--
-- Each message is [tag :: Word8] [length :: Int32] [payload].
-- The length field includes itself (4 bytes) but not the tag byte.
--
-- We pre-compute the payload size from the message fields, then write
-- tag + length + payload in a single Builder pass. This avoids the
-- previous approach of materializing the payload to a ByteString just
-- to compute its length (which caused a double-copy).

encodeFrontendMsg :: FrontendMsg -> Builder
encodeFrontendMsg = \case
  Startup params -> encodeStartup params
  Parse name sql oids ->
    let !sz = cstringSize name + cstringSize sql + 2 + V.length oids * 4
     in tag 'P' sz <> cstring name <> cstring sql
          <> B.int16BE (fromIntegral (V.length oids) :: Int16)
          <> V.foldl' (\b oid -> b <> B.word32BE oid) mempty oids
  Bind portal stmt pfmts vals rfmts ->
    let !sz = cstringSize portal + cstringSize stmt
            + formatCodesSize pfmts
            + 2 + paramValuesSize vals
            + formatCodesSize rfmts
     in tag 'B' sz <> cstring portal <> cstring stmt
          <> encodeFormatCodes pfmts
          <> B.int16BE (fromIntegral (V.length vals) :: Int16)
          <> V.foldl' (\b mv -> b <> encodeParamValue mv) mempty vals
          <> encodeFormatCodes rfmts
  Describe target name ->
    let !sz = 1 + cstringSize name
     in tag 'D' sz <> B.char8 (targetChar target) <> cstring name
  Execute portal maxRows ->
    let !sz = cstringSize portal + 4
     in tag 'E' sz <> cstring portal <> B.int32BE maxRows
  Close target name ->
    let !sz = 1 + cstringSize name
     in tag 'C' sz <> B.char8 (targetChar target) <> cstring name
  Sync -> tag 'S' 0
  Flush -> tag 'H' 0
  Terminate -> tag 'X' 0
  Query sql ->
    let !sz = cstringSize sql
     in tag 'Q' sz <> cstring sql
  PasswordMessage pw ->
    let !sz = cstringSize pw
     in tag 'p' sz <> cstring pw
  SASLInitialResponse mech clientFirst ->
    let !sz = cstringSize mech + 4 + BS.length clientFirst
     in tag 'p' sz <> cstring mech
          <> B.int32BE (fromIntegral (BS.length clientFirst))
          <> B.byteString clientFirst
  SASLResponse msg ->
    let !sz = BS.length msg
     in tag 'p' sz <> B.byteString msg
  CopyData dat ->
    let !sz = BS.length dat
     in tag 'd' sz <> B.byteString dat
  CopyDone -> tag 'c' 0
  CopyFail msg ->
    let !sz = cstringSize msg
     in tag 'f' sz <> cstring msg
{-# INLINE encodeFrontendMsg #-}

-- | Write tag byte + length (payload size + 4 for the length field itself).
tag :: Char -> Int -> Builder
tag c payloadSize =
  B.char8 c <> B.int32BE (fromIntegral (payloadSize + 4) :: Int32)
{-# INLINE tag #-}

-- Size computation helpers ------------------------------------------------

-- | Size of a NUL-terminated C string: data bytes + 1 NUL byte.
cstringSize :: ByteString -> Int
cstringSize bs = BS.length bs + 1
{-# INLINE cstringSize #-}

-- | Size of format codes: 2 (count) + 2 per code.
formatCodesSize :: Vector FormatCode -> Int
formatCodesSize fmts = 2 + V.length fmts * 2
{-# INLINE formatCodesSize #-}

-- | Size of parameter values: sum of (4 + data) per non-NULL, 4 per NULL.
paramValuesSize :: Vector (Maybe ByteString) -> Int
paramValuesSize = V.foldl' (\acc mv -> acc + paramValueSize mv) 0
{-# INLINE paramValuesSize #-}

paramValueSize :: Maybe ByteString -> Int
paramValueSize Nothing = 4
paramValueSize (Just bs) = 4 + BS.length bs
{-# INLINE paramValueSize #-}

-- Payload encoding helpers ------------------------------------------------

encodeFormatCodes :: Vector FormatCode -> Builder
encodeFormatCodes fmts =
  B.int16BE (fromIntegral (V.length fmts) :: Int16)
    <> V.foldl' (\b fc -> b <> B.int16BE (formatCodeToInt16 fc)) mempty fmts
{-# INLINE encodeFormatCodes #-}

formatCodeToInt16 :: FormatCode -> Int16
formatCodeToInt16 TextFormat = 0
formatCodeToInt16 BinaryFormat = 1
{-# INLINE formatCodeToInt16 #-}

encodeParamValue :: Maybe ByteString -> Builder
encodeParamValue Nothing = B.int32BE (-1)
encodeParamValue (Just bs) =
  B.int32BE (fromIntegral (BS.length bs))
    <> B.byteString bs
{-# INLINE encodeParamValue #-}

targetChar :: DescribeTarget -> Char
targetChar DescribeStatement = 'S'
targetChar DescribePortal = 'P'
{-# INLINE targetChar #-}

-- | NUL-terminated C string.
cstring :: ByteString -> Builder
cstring bs = B.byteString bs <> B.word8 0
{-# INLINE cstring #-}

-- Startup message ---------------------------------------------------------
-- Startup has a different format: [length :: Int32] [protocol] [params] [NUL]
-- No tag byte. We still need to materialize to compute the length here,
-- but startup is called once per connection, not on the hot path.

encodeStartup :: StartupParams -> Builder
encodeStartup StartupParams {..} =
  let params =
        cstring "user" <> cstring spUser
          <> cstring "database" <> cstring spDatabase
          <> (if BS.null spAppName then mempty else cstring "application_name" <> cstring spAppName)
          <> mconcat [cstring k <> cstring v | (k, v) <- spExtraParams]
          <> B.word8 0 -- terminator
      paramsBs = LBS.toStrict (B.toLazyByteString params)
      len = fromIntegral (4 + 4 + BS.length paramsBs) :: Int32
      protocolVersion = (3 :: Int32) `shiftL` 16 -- 3.0
   in B.int32BE len <> B.int32BE protocolVersion <> B.byteString paramsBs
