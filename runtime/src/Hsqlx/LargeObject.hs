-- | PostgreSQL large object API.
--
-- Large objects provide a streaming interface for binary data too large
-- to fit in a single column value. Operations must occur within a
-- transaction.
--
-- @
-- withTransaction pool $ \\tx -> do
--   oid <- loCreate (txConn tx)
--   fd <- loOpen (txConn tx) oid WriteMode
--   loWrite (txConn tx) fd someBytes
--   loClose (txConn tx) fd
-- @
module Hsqlx.LargeObject
  ( -- * Types
    LoFd (..)
  , LoMode (..)
    -- * Create / delete
  , loCreate
  , loUnlink
    -- * Open / close
  , loOpen
  , loClose
  , withLargeObject
    -- * Read / write
  , loRead
  , loWrite
    -- * Seek / tell
  , loSeek
  , loTell
  , loTruncate
    -- * Import / export
  , loImport
  , loExport
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32, Int64)
import Data.Word (Word32)
import PgWire.Connection (Connection, escapeLiteral, simpleQuery)

-- | A large object file descriptor, returned by 'loOpen'.
newtype LoFd = LoFd { unLoFd :: Int32 }
  deriving stock (Show, Eq)

-- | Access mode for 'loOpen'.
data LoMode
  = ReadMode
  | WriteMode
  | ReadWriteMode
  deriving stock (Show, Eq)

loModeToInt :: LoMode -> Int32
loModeToInt ReadMode = 0x00040000 -- INV_READ
loModeToInt WriteMode = 0x00020000 -- INV_WRITE
loModeToInt ReadWriteMode = 0x00060000 -- INV_READ | INV_WRITE

-- | Create a new large object with a system-assigned OID.
-- Returns the OID of the new object.
loCreate :: Connection -> IO Word32
loCreate conn = do
  (rows, _) <- simpleQuery conn "SELECT lo_create(0)"
  case rows of
    [[Just oidBs]] -> case BS8.readInt oidBs of
      Just (n, _) -> pure (fromIntegral n)
      Nothing -> fail "loCreate: could not parse OID"
    _ -> fail "loCreate: unexpected result"

-- | Delete a large object.
loUnlink :: Connection -> Word32 -> IO ()
loUnlink conn oid = do
  _ <- simpleQuery conn ("SELECT lo_unlink(" <> BS8.pack (show oid) <> ")")
  pure ()

-- | Open a large object for reading and/or writing.
-- Must be called within a transaction.
loOpen :: Connection -> Word32 -> LoMode -> IO LoFd
loOpen conn oid mode = do
  (rows, _) <- simpleQuery conn
    ("SELECT lo_open(" <> BS8.pack (show oid) <> ", " <> BS8.pack (show (loModeToInt mode)) <> ")")
  case rows of
    [[Just fdBs]] -> case BS8.readInt fdBs of
      Just (n, _) -> pure (LoFd (fromIntegral n))
      Nothing -> fail "loOpen: could not parse fd"
    _ -> fail "loOpen: unexpected result"

-- | Close a large object file descriptor.
loClose :: Connection -> LoFd -> IO ()
loClose conn (LoFd fd) = do
  _ <- simpleQuery conn ("SELECT lo_close(" <> BS8.pack (show fd) <> ")")
  pure ()

-- | Open a large object, run an action, then close. Guarantees the file
-- descriptor is closed even if the action throws an exception.
-- Must be called within a transaction.
withLargeObject :: Connection -> Word32 -> LoMode -> (LoFd -> IO a) -> IO a
withLargeObject conn oid mode =
  bracket (loOpen conn oid mode) (loClose conn)

-- | Read up to @n@ bytes from a large object.
loRead :: Connection -> LoFd -> Int32 -> IO ByteString
loRead conn (LoFd fd) n = do
  (rows, _) <- simpleQuery conn
    ("SELECT loread(" <> BS8.pack (show fd) <> ", " <> BS8.pack (show n) <> ")")
  case rows of
    [[Just bs]] -> pure (unescapeBytea bs)
    _ -> fail "loRead: unexpected result"

-- | Write bytes to a large object. Returns number of bytes written.
loWrite :: Connection -> LoFd -> ByteString -> IO Int32
loWrite conn (LoFd fd) bs = do
  let escaped = escapeBytea bs
  (rows, _) <- simpleQuery conn
    ("SELECT lowrite(" <> BS8.pack (show fd) <> ", " <> escaped <> ")")
  case rows of
    [[Just nBs]] -> case BS8.readInt nBs of
      Just (n, _) -> pure (fromIntegral n)
      Nothing -> fail "loWrite: could not parse result"
    _ -> fail "loWrite: unexpected result"

-- | Seek to a position in a large object. Returns the new position.
loSeek :: Connection -> LoFd -> Int64 -> Int32 -> IO Int64
loSeek conn (LoFd fd) offset whence = do
  (rows, _) <- simpleQuery conn
    ("SELECT lo_lseek64(" <> BS8.pack (show fd) <> ", " <> BS8.pack (show offset) <> ", " <> BS8.pack (show whence) <> ")")
  case rows of
    [[Just posBs]] -> case BS8.readInteger posBs of
      Just (n, _) -> pure (fromIntegral n)
      Nothing -> fail "loSeek: could not parse position"
    _ -> fail "loSeek: unexpected result"

-- | Get the current position in a large object.
loTell :: Connection -> LoFd -> IO Int64
loTell conn (LoFd fd) = do
  (rows, _) <- simpleQuery conn ("SELECT lo_tell64(" <> BS8.pack (show fd) <> ")")
  case rows of
    [[Just posBs]] -> case BS8.readInteger posBs of
      Just (n, _) -> pure (fromIntegral n)
      Nothing -> fail "loTell: could not parse position"
    _ -> fail "loTell: unexpected result"

-- | Truncate a large object to the given length.
loTruncate :: Connection -> LoFd -> Int64 -> IO ()
loTruncate conn (LoFd fd) len = do
  _ <- simpleQuery conn
    ("SELECT lo_truncate64(" <> BS8.pack (show fd) <> ", " <> BS8.pack (show len) <> ")")
  pure ()

-- | Import a file from the server filesystem as a large object.
-- Returns the OID of the new object.
loImport :: Connection -> ByteString -> IO Word32
loImport conn path = do
  (rows, _) <- simpleQuery conn ("SELECT lo_import(" <> escapeLiteral conn path <> ")")
  case rows of
    [[Just oidBs]] -> case BS8.readInt oidBs of
      Just (n, _) -> pure (fromIntegral n)
      Nothing -> fail "loImport: could not parse OID"
    _ -> fail "loImport: unexpected result"

-- | Export a large object to a file on the server filesystem.
loExport :: Connection -> Word32 -> ByteString -> IO ()
loExport conn oid path = do
  _ <- simpleQuery conn
    ("SELECT lo_export(" <> BS8.pack (show oid) <> ", " <> escapeLiteral conn path <> ")")
  pure ()

-- Simple bytea escape/unescape for the text protocol used by simpleQuery.
-- In the simple query protocol, bytea is returned as hex or escape format.
escapeBytea :: ByteString -> ByteString
escapeBytea bs = "'\\x" <> BS8.pack (concatMap toHex (BS.unpack bs)) <> "'"
  where
    toHex w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

-- Unescape hex-encoded bytea from simple query response.
unescapeBytea :: ByteString -> ByteString
unescapeBytea bs
  | "\\x" `BS.isPrefixOf` bs = decodeHex (BS.drop 2 bs)
  | otherwise = bs -- already raw or escape format
  where
    decodeHex hex = BS.pack (go (BS.unpack hex))
    go [] = []
    go [_] = [] -- odd number of chars, ignore trailing
    go (h : l : rest) = fromHex h l : go rest
    fromHex h l = fromIntegral (hexVal h * 16 + hexVal l)
    hexVal c
      | c >= 48 && c <= 57 = c - 48      -- '0'-'9'
      | c >= 97 && c <= 102 = c - 87     -- 'a'-'f'
      | c >= 65 && c <= 70 = c - 55      -- 'A'-'F'
      | otherwise = 0
