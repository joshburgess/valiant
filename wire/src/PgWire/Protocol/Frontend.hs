-- | Frontend (client-to-server) message types from the PostgreSQL v3
-- wire protocol.
--
-- __Audience:__ exposed for downstream library authors that need to
-- assemble protocol messages directly. Most application code uses
-- "PgWire.Connection" instead.
module PgWire.Protocol.Frontend
  ( FrontendMsg (..)
  , StartupParams (..)
  , DescribeTarget (..)
  , FormatCode (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Word (Word32)

-- | Format codes for parameter/result encoding.
data FormatCode = TextFormat | BinaryFormat
  deriving stock (Show, Eq)

-- | Target for a Describe message.
data DescribeTarget = DescribeStatement | DescribePortal
  deriving stock (Show, Eq)

-- | Parameters for the startup message.
data StartupParams = StartupParams
  { spUser :: ByteString
  , spDatabase :: ByteString
  , spAppName :: ByteString
  , spExtraParams :: [(ByteString, ByteString)]
  }
  deriving stock (Show)

-- | Client-to-server (frontend) wire protocol messages.
data FrontendMsg
  = -- | Initial startup (no tag byte)
    Startup StartupParams
  | -- | Parse a prepared statement
    Parse
      ByteString
      -- ^ Statement name (empty = unnamed)
      ByteString
      -- ^ SQL query
      (Vector Word32)
      -- ^ Parameter type OIDs (0 = let server infer)
  | -- | Bind parameters to a prepared statement
    Bind
      ByteString
      -- ^ Portal name (empty = unnamed)
      ByteString
      -- ^ Statement name
      (Vector FormatCode)
      -- ^ Parameter format codes
      (Vector (Maybe ByteString))
      -- ^ Parameter values (Nothing = NULL)
      (Vector FormatCode)
      -- ^ Result format codes
  | -- | Describe a prepared statement or portal
    Describe DescribeTarget ByteString
  | -- | Execute a portal
    Execute
      ByteString
      -- ^ Portal name
      Int32
      -- ^ Max rows (0 = unlimited)
  | -- | Close a prepared statement or portal
    Close DescribeTarget ByteString
  | -- | Sync — end of an extended query sequence
    Sync
  | -- | Flush — request server to send pending output
    Flush
  | -- | Terminate the connection
    Terminate
  | -- | Simple query protocol
    Query ByteString
  | -- | Password message (cleartext or MD5)
    PasswordMessage ByteString
  | -- | SASL initial response
    SASLInitialResponse
      ByteString
      -- ^ Mechanism name
      ByteString
      -- ^ Client first message
  | -- | SASL response (subsequent messages)
    SASLResponse ByteString
  | -- | CopyData — a chunk of COPY data
    CopyData ByteString
  | -- | CopyDone — end of COPY data from client
    CopyDone
  | -- | CopyFail — client aborts COPY
    CopyFail ByteString
  deriving stock (Show)
