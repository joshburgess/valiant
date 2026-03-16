module PgWire.Protocol.Backend
  ( BackendMsg (..)
  , FieldInfo (..)
  , PgError (..)
  , PgNotice (..)
  , TxStatus (..)
  , CommandTag (..)
  , AuthType (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Vector (Vector)
import Data.Word (Word32)

-- | Transaction status indicator.
data TxStatus
  = TxIdle
  | TxInTransaction
  | TxFailed
  deriving stock (Show, Eq)

-- | Parsed command completion tag.
data CommandTag
  = SelectTag Int64
  | InsertTag Int64
  | UpdateTag Int64
  | DeleteTag Int64
  | OtherTag ByteString
  deriving stock (Show, Eq)

-- | Authentication sub-types.
data AuthType
  = AuthOk
  | AuthCleartextPassword
  | AuthMD5Password ByteString -- 4-byte salt
  | AuthSASL [ByteString] -- mechanism names
  | AuthSASLContinue ByteString -- server data
  | AuthSASLFinal ByteString -- server signature
  deriving stock (Show, Eq)

-- | Column metadata from RowDescription.
data FieldInfo = FieldInfo
  { fiName :: ByteString
  , fiTableOid :: Word32
  , fiColumnNum :: Int16
  , fiTypeOid :: Word32
  , fiTypeSize :: Int16
  , fiTypeMod :: Int32
  , fiFormatCode :: Int16
  }
  deriving stock (Show, Eq)

-- | Structured error from ErrorResponse.
-- See https://www.postgresql.org/docs/current/protocol-error-fields.html
data PgError = PgError
  { pgSeverity :: ByteString
  , pgCode :: ByteString
  , pgMessage :: ByteString
  , pgDetail :: Maybe ByteString
  , pgHint :: Maybe ByteString
  , pgPosition :: Maybe Int32
  , pgInternalPosition :: Maybe Int32
  , pgInternalQuery :: Maybe ByteString
  , pgWhere :: Maybe ByteString
  , pgSchema :: Maybe ByteString
  , pgTable :: Maybe ByteString
  , pgColumn :: Maybe ByteString
  , pgDataType :: Maybe ByteString
  , pgConstraint :: Maybe ByteString
  , pgFile :: Maybe ByteString
  , pgLine :: Maybe Int32
  , pgRoutine :: Maybe ByteString
  }
  deriving stock (Show, Eq)

-- | Structured notice from NoticeResponse.
newtype PgNotice = PgNotice {unPgNotice :: PgError}
  deriving stock (Show, Eq)

-- | Server-to-client (backend) wire protocol messages.
data BackendMsg
  = Authentication AuthType
  | ParameterStatus ByteString ByteString
  | BackendKeyData Int32 Int32
  | ReadyForQuery TxStatus
  | ParseComplete
  | BindComplete
  | CloseComplete
  | NoData
  | EmptyQueryResponse
  | ParameterDescription (Vector Word32)
  | RowDescription (Vector FieldInfo)
  | DataRow (Vector (Maybe ByteString))
  | CommandComplete CommandTag
  | ErrorResponse PgError
  | NoticeResponse PgNotice
  | NotificationResponse Int32 ByteString ByteString
  | -- | CopyInResponse: server is ready to receive COPY data
    CopyInResponse Int8Format (Vector Int16)
  | -- | CopyOutResponse: server will send COPY data
    CopyOutResponse Int8Format (Vector Int16)
  | -- | CopyData: a chunk of COPY data from the server
    CopyDataMsg ByteString
  | -- | CopyDone: end of COPY data from server
    CopyDoneMsg
  deriving stock (Show, Eq)

-- | COPY format code (0 = text, 1 = binary)
type Int8Format = Int16
