-- | SCRAM-SHA-256 authentication (RFC 5802 / RFC 7677), with optional
-- @tls-server-end-point@ channel binding.
--
-- __Audience:__ exposed for downstream library authors. The standard
-- authentication flow is driven from "PgWire.Connection"; this module
-- is only useful if you are reimplementing handshake.
module PgWire.Auth.ScramSHA256
  ( scramAuth
  , scramAuthWithChannelBinding
  ) where

import Control.Monad (unless)
import Text.Read (readMaybe)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.KDF.PBKDF2 qualified as PBKDF2
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random (getRandomBytes)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BS8
import Data.Bits (xor)
import PgWire.Error (PgWireError (..), throwPgWire)
import PgWire.Protocol.Backend (AuthType (..), BackendMsg (..), PgError (..))
import PgWire.Protocol.Frontend (FrontendMsg (..))
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg)

-- | Perform SCRAM-SHA-256 authentication without channel binding.
scramAuth :: WireConn -> ByteString -> ByteString -> IO ()
scramAuth wc user password = scramAuthInternal wc user password Nothing

-- | Perform SCRAM-SHA-256-PLUS authentication with tls-server-end-point
-- channel binding. The ByteString argument is the SHA-256 hash of the
-- server's TLS certificate (DER encoded).
scramAuthWithChannelBinding :: WireConn -> ByteString -> ByteString -> ByteString -> IO ()
scramAuthWithChannelBinding wc user password certHash =
  scramAuthInternal wc user password (Just certHash)

scramAuthInternal :: WireConn -> ByteString -> ByteString -> Maybe ByteString -> IO ()
scramAuthInternal wc user password mCertHash = do
  nonceBytes <- getRandomBytes 18 :: IO ByteString
  let clientNonce = B64.encode nonceBytes

      -- Channel binding header:
      --   "n,," for no channel binding
      --   "p=tls-server-end-point,," for tls-server-end-point
      (gs2Header, cbData, mechName) = case mCertHash of
        Nothing ->
          ("n,,", "n,,", "SCRAM-SHA-256")
        Just certHash ->
          let hdr = "p=tls-server-end-point,,"
           in (hdr, hdr <> certHash, "SCRAM-SHA-256-PLUS")

      clientFirstBare = "n=" <> user <> ",r=" <> clientNonce
      clientFirstMsg = gs2Header <> clientFirstBare

  -- Send SASL initial response
  sendFrontendMsg wc (SASLInitialResponse mechName clientFirstMsg)

  -- Receive server first message
  msg1 <- recvBackendMsg wc
  serverFirstMsg <- case msg1 of
    Authentication (AuthSASLContinue serverData) -> pure serverData
    Authentication AuthOk -> pure ""
    ErrorResponse err -> throwPgWire (AuthError (pgMessage err))
    other -> throwPgWire (AuthError ("Unexpected message during SCRAM: " <> BS8.pack (show other)))

  -- Parse server first message: r=<nonce>,s=<salt>,i=<iterations>
  let serverFields = parseScramFields serverFirstMsg
  serverNonce <- lookupField "r" serverFields
  saltB64 <- lookupField "s" serverFields
  iterStr <- lookupField "i" serverFields

  iterations <- case readMaybe (BS8.unpack iterStr) :: Maybe Int of
    Just n | n > 0 -> pure n
    _ -> throwPgWire (AuthError ("Bad SCRAM iteration count: " <> iterStr))
  salt <- case B64.decode saltB64 of
    Left err -> throwPgWire (AuthError ("Bad salt base64: " <> BS8.pack err))
    Right s -> pure s

  -- Verify server nonce starts with our client nonce
  unless (clientNonce `BS.isPrefixOf` serverNonce) $
    throwPgWire (AuthError "Server nonce doesn't start with client nonce")

  -- Compute proofs
  let saltedPassword = hi password salt iterations
      clientKey = hmacSHA256 saltedPassword "Client Key"
      storedKey = hashSHA256 clientKey
      serverKey = hmacSHA256 saltedPassword "Server Key"

      channelBinding = B64.encode cbData
      clientFinalWithoutProof = "c=" <> channelBinding <> ",r=" <> serverNonce
      authMessage = clientFirstBare <> "," <> serverFirstMsg <> "," <> clientFinalWithoutProof

      clientSignature = hmacSHA256 storedKey authMessage
      clientProof = BS.pack (BS.zipWith xor clientKey clientSignature)
      serverSignature = hmacSHA256 serverKey authMessage

      clientFinalMsg = clientFinalWithoutProof <> ",p=" <> B64.encode clientProof

  -- Send client final message
  sendFrontendMsg wc (SASLResponse clientFinalMsg)

  -- Receive server final message
  msg2 <- recvBackendMsg wc
  case msg2 of
    Authentication (AuthSASLFinal serverData) -> do
      let sFields = parseScramFields serverData
      case lookup "v" sFields of
        Nothing -> throwPgWire (AuthError "Missing server signature in SASL final")
        Just sigB64 -> case B64.decode sigB64 of
          Left err -> throwPgWire (AuthError ("Bad server sig base64: " <> BS8.pack err))
          Right sig
            | not (BA.constEq sig serverSignature) ->
                throwPgWire (AuthError "Server signature mismatch")
            | otherwise -> pure ()
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwPgWire (AuthError (pgMessage err))
    other -> throwPgWire (AuthError ("Unexpected message during SCRAM final: " <> BS8.pack (show other)))

  -- Wait for AuthOk
  msg3 <- recvBackendMsg wc
  case msg3 of
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwPgWire (AuthError (pgMessage err))
    _ -> throwPgWire (AuthError "Expected AuthOk after SCRAM")

-- Crypto helpers ----------------------------------------------------------

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = BA.convert (hmac key msg :: HMAC SHA256)
{-# INLINE hmacSHA256 #-}

hashSHA256 :: ByteString -> ByteString
hashSHA256 bs = BA.convert (hashWith SHA256 bs)
{-# INLINE hashSHA256 #-}

-- | PBKDF2-SHA256 (called "Hi" in SCRAM spec).
hi :: ByteString -> ByteString -> Int -> ByteString
hi password salt iterations =
  PBKDF2.generate
    (PBKDF2.prfHMAC SHA256)
    (PBKDF2.Parameters iterations 32)
    password
    salt

-- SCRAM field parsing -----------------------------------------------------

parseScramFields :: ByteString -> [(ByteString, ByteString)]
parseScramFields bs =
  [ (key, val)
  | field <- BS8.split ',' bs
  , let (key, rest) = BS.break (== 61) field -- '='
  , not (BS.null rest)
  , let val = BS.drop 1 rest
  ]

lookupField :: ByteString -> [(ByteString, ByteString)] -> IO ByteString
lookupField key fields =
  case lookup key fields of
    Nothing -> throwPgWire (AuthError ("Missing SCRAM field: " <> key))
    Just v -> pure v
