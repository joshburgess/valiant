module Valiant.CLI.Error
  ( ValiantCliError (..)
  , PgErrorDetail (..)
  , dieWithError
  , renderError
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.PostgreSQL.LibPQ (Oid (..))
import System.Exit (exitFailure)
import System.IO (stderr)

-- | Structured error from Postgres.
data PgErrorDetail = PgErrorDetail
  { pgMessage :: Text
  , pgDetail :: Maybe Text
  , pgHint :: Maybe Text
  , pgPosition :: Maybe Int
  }
  deriving stock (Show)

-- | All CLI failure modes.
data ValiantCliError
  = ErrNoDatabaseUrl
  | ErrConnectionFailed Text
  | ErrSqlDirNotFound FilePath
  | ErrNoSqlFiles FilePath
  | ErrDescribeFailed FilePath PgErrorDetail
  | ErrUnknownOid FilePath Text Oid
  | ErrCacheReadFailed FilePath String
  | ErrStaleCacheFile FilePath Text Text
  | ErrMissingCacheFile FilePath
  deriving stock (Show)

-- | Render an error to human-readable text.
renderError :: ValiantCliError -> Text
renderError = \case
  ErrNoDatabaseUrl ->
    T.unlines
      [ "valiant: DATABASE_URL is not set."
      , ""
      , "  Set it in your environment or in a .env file:"
      , "    DATABASE_URL=postgres://user:pass@localhost:5432/mydb"
      ]
  ErrConnectionFailed msg ->
    "valiant: Failed to connect to database: " <> msg
  ErrSqlDirNotFound dir ->
    "valiant: SQL directory not found: " <> T.pack dir
  ErrNoSqlFiles dir ->
    "valiant: No .sql files found in " <> T.pack dir
  ErrDescribeFailed path detail ->
    T.unlines $
      [ "valiant: " <> T.pack path <> ": FAILED"
      , "  ERROR: " <> pgMessage detail
      ]
        <> maybe [] (\d -> ["  DETAIL: " <> d]) (pgDetail detail)
        <> maybe [] (\h -> ["  HINT: " <> h]) (pgHint detail)
  ErrUnknownOid path col (Oid oid) ->
    T.unlines
      [ "valiant: " <> T.pack path <> ": unknown Postgres type"
      , "  Column \"" <> col <> "\" has OID " <> T.pack (show oid)
      , "  which valiant doesn't know how to map to a Haskell type."
      , ""
      , "  Cast the column in SQL or register a custom type mapping."
      ]
  ErrCacheReadFailed path msg ->
    "valiant: Failed to read cache file " <> T.pack path <> ": " <> T.pack msg
  ErrStaleCacheFile path cached current ->
    T.unlines
      [ "valiant: " <> T.pack path <> ": STALE"
      , "  SQL content has changed since last prepare."
      , "  Cache hash:   " <> cached
      , "  Current hash: " <> current
      ]
  ErrMissingCacheFile path ->
    T.unlines
      [ "valiant: " <> T.pack path <> ": MISSING"
      , "  No cached metadata found. Run `valiant prepare`."
      ]

-- | Print an error to stderr and exit with failure.
dieWithError :: ValiantCliError -> IO a
dieWithError err = do
  TIO.hPutStrLn stderr (renderError err)
  exitFailure
