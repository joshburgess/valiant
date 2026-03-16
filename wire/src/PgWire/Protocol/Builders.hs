module PgWire.Protocol.Builders
  ( buildFrontendMsg
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
import Data.Word (Word32)
import PgWire.Protocol.Frontend

-- | Build a complete frontend message as a strict 'ByteString'.
buildFrontendMsg :: FrontendMsg -> ByteString
buildFrontendMsg = LBS.toStrict . B.toLazyByteString . encodeFrontendMsg
{-# INLINE buildFrontendMsg #-}

-- | Build the startup message (special: no tag byte).
buildStartup :: StartupParams -> ByteString
buildStartup = LBS.toStrict . B.toLazyByteString . encodeStartup

-- Encoding ----------------------------------------------------------------

encodeFrontendMsg :: FrontendMsg -> Builder
encodeFrontendMsg = \case
  Startup params -> encodeStartup params
  Parse name sql oids -> withTag 'P' $ encodeparse name sql oids
  Bind portal stmt pfmts vals rfmts -> withTag 'B' $ encodeBind portal stmt pfmts vals rfmts
  Describe target name -> withTag 'D' $ encodeDescribe target name
  Execute portal maxRows -> withTag 'E' $ encodeExecute portal maxRows
  Close target name -> withTag 'C' $ encodeDescribe target name -- same format as Describe
  Sync -> withTag 'S' mempty
  Flush -> withTag 'H' mempty
  Terminate -> withTag 'X' mempty
  Query sql -> withTag 'Q' $ cstring sql
  PasswordMessage pw -> withTag 'p' $ cstring pw
  SASLInitialResponse mech clientFirst ->
    withTag 'p' $
      cstring mech
        <> B.int32BE (fromIntegral (BS.length clientFirst))
        <> B.byteString clientFirst
  SASLResponse msg -> withTag 'p' $ B.byteString msg
  CopyData dat -> withTag 'd' $ B.byteString dat
  CopyDone -> withTag 'c' mempty
  CopyFail msg -> withTag 'f' $ cstring msg

-- | Wrap a payload with [tag][length] header. Length includes itself (4 bytes).
withTag :: Char -> Builder -> Builder
withTag tag payload =
  let payloadBs = LBS.toStrict (B.toLazyByteString payload)
      len = fromIntegral (BS.length payloadBs + 4) :: Int32
   in B.char8 tag <> B.int32BE len <> B.byteString payloadBs
{-# INLINE withTag #-}

-- Startup message: [length :: Int32] [protocol :: Int32] [params] [NUL]
encodeStartup :: StartupParams -> Builder
encodeStartup StartupParams {..} =
  let params =
        cstring "user" <> cstring spUser
          <> cstring "database" <> cstring spDatabase
          <> (if BS.null spAppName then mempty else cstring "application_name" <> cstring spAppName)
          <> mconcat [cstring k <> cstring v | (k, v) <- spExtraParams]
          <> B.word8 0 -- terminator
      paramsBs = LBS.toStrict (B.toLazyByteString params)
      -- length includes itself (4) + protocol version (4) + params
      len = fromIntegral (4 + 4 + BS.length paramsBs) :: Int32
      protocolVersion = (3 :: Int32) `shiftL` 16 -- 3.0
   in B.int32BE len <> B.int32BE protocolVersion <> B.byteString paramsBs

-- Parse: [name NUL] [sql NUL] [nparams :: Int16] [oid :: Int32 ...]
encodeparse :: ByteString -> ByteString -> Vector Word32 -> Builder
encodeparse name sql oids =
  cstring name
    <> cstring sql
    <> B.int16BE (fromIntegral (V.length oids) :: Int16)
    <> V.foldl' (\b oid -> b <> B.word32BE oid) mempty oids

-- Bind: [portal NUL] [stmt NUL] [nfmts :: Int16] [fmts :: Int16 ...]
--       [nparams :: Int16] [len :: Int32, data | -1 for NULL ...]
--       [nrfmts :: Int16] [rfmts :: Int16 ...]
encodeBind :: ByteString -> ByteString -> Vector FormatCode -> Vector (Maybe ByteString) -> Vector FormatCode -> Builder
encodeBind portal stmt pfmts vals rfmts =
  cstring portal
    <> cstring stmt
    <> encodeFormatCodes pfmts
    <> B.int16BE (fromIntegral (V.length vals) :: Int16)
    <> V.foldl' (\b mv -> b <> encodeParamValue mv) mempty vals
    <> encodeFormatCodes rfmts

encodeFormatCodes :: Vector FormatCode -> Builder
encodeFormatCodes fmts =
  B.int16BE (fromIntegral (V.length fmts) :: Int16)
    <> V.foldl' (\b fc -> b <> B.int16BE (formatCodeToInt16 fc)) mempty fmts

formatCodeToInt16 :: FormatCode -> Int16
formatCodeToInt16 TextFormat = 0
formatCodeToInt16 BinaryFormat = 1

encodeParamValue :: Maybe ByteString -> Builder
encodeParamValue Nothing = B.int32BE (-1)
encodeParamValue (Just bs) =
  B.int32BE (fromIntegral (BS.length bs))
    <> B.byteString bs

-- Describe/Close: [target :: Char] [name NUL]
encodeDescribe :: DescribeTarget -> ByteString -> Builder
encodeDescribe target name =
  B.char8 (targetChar target) <> cstring name

targetChar :: DescribeTarget -> Char
targetChar DescribeStatement = 'S'
targetChar DescribePortal = 'P'

-- Execute: [portal NUL] [max_rows :: Int32]
encodeExecute :: ByteString -> Int32 -> Builder
encodeExecute portal maxRows =
  cstring portal <> B.int32BE maxRows

-- Helpers -----------------------------------------------------------------

-- | NUL-terminated C string.
cstring :: ByteString -> Builder
cstring bs = B.byteString bs <> B.word8 0
{-# INLINE cstring #-}
