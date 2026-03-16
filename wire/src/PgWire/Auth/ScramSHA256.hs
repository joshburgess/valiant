module PgWire.Auth.ScramSHA256
  ( scramAuth
  ) where

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
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend (AuthType (..), BackendMsg (..), PgError (..))
import PgWire.Protocol.Frontend (FrontendMsg (..))
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg)

-- | Perform SCRAM-SHA-256 authentication.
scramAuth :: WireConn -> ByteString -> ByteString -> IO ()
scramAuth wc user password = do
  -- Client first message (bare, without "n,,")
  nonceBytes <- getRandomBytes 18 :: IO ByteString
  let clientNonce = B64.encode nonceBytes
      clientFirstBare = "n=" <> user <> ",r=" <> clientNonce
      clientFirstMsg = "n,," <> clientFirstBare

  -- Send SASL initial response
  sendFrontendMsg wc (SASLInitialResponse "SCRAM-SHA-256" clientFirstMsg)

  -- Receive server first message
  msg1 <- recvBackendMsg wc
  serverFirstMsg <- case msg1 of
    Authentication (AuthSASLContinue serverData) -> pure serverData
    Authentication (AuthOk) -> pure "" -- some PG versions might accept immediately
    ErrorResponse err -> throwHsqlx (AuthError (pgMessage err))
    other -> throwHsqlx (AuthError ("Unexpected message during SCRAM: " <> BS8.pack (show other)))

  -- Parse server first message: r=<nonce>,s=<salt>,i=<iterations>
  let serverFields = parseScramFields serverFirstMsg
  serverNonce <- lookupField "r" serverFields
  saltB64 <- lookupField "s" serverFields
  iterStr <- lookupField "i" serverFields

  let iterations = read (BS8.unpack iterStr) :: Int
  salt <- case B64.decode saltB64 of
    Left err -> throwHsqlx (AuthError ("Bad salt base64: " <> BS8.pack err))
    Right s -> pure s

  -- Verify server nonce starts with our client nonce
  if not (clientNonce `BS.isPrefixOf` serverNonce)
    then throwHsqlx (AuthError "Server nonce doesn't start with client nonce")
    else pure ()

  -- Compute proofs
  let saltedPassword = hi password salt iterations
      clientKey = hmacSHA256 saltedPassword "Client Key"
      storedKey = hashSHA256 clientKey
      serverKey = hmacSHA256 saltedPassword "Server Key"

      channelBinding = B64.encode "n,,"
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
        Nothing -> throwHsqlx (AuthError "Missing server signature in SASL final")
        Just sigB64 -> case B64.decode sigB64 of
          Left err -> throwHsqlx (AuthError ("Bad server sig base64: " <> BS8.pack err))
          Right sig
            | sig /= serverSignature ->
                throwHsqlx (AuthError "Server signature mismatch")
            | otherwise -> pure ()
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwHsqlx (AuthError (pgMessage err))
    other -> throwHsqlx (AuthError ("Unexpected message during SCRAM final: " <> BS8.pack (show other)))

  -- Wait for AuthOk
  msg3 <- recvBackendMsg wc
  case msg3 of
    Authentication AuthOk -> pure ()
    ErrorResponse err -> throwHsqlx (AuthError (pgMessage err))
    _ -> throwHsqlx (AuthError "Expected AuthOk after SCRAM")

-- Crypto helpers ----------------------------------------------------------

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key msg = BA.convert (hmac key msg :: HMAC SHA256)

hashSHA256 :: ByteString -> ByteString
hashSHA256 bs = BA.convert (hashWith SHA256 bs)

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
    Nothing -> throwHsqlx (AuthError ("Missing SCRAM field: " <> key))
    Just v -> pure v
