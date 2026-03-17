-- | Configuration types for PostgreSQL connections.
--
-- Supports both URI format (@postgres:\/\/user:pass\@host:port\/db@) and
-- key=value format (@host=localhost port=5432 dbname=mydb@). Multi-host
-- failover, TLS modes, and session attribute targeting are configurable.
module PgWire.Connection.Config
  ( ConnConfig (..)
  , TlsMode (..)
  , TargetSessionAttrs (..)
  , defaultConnConfig
  , parseConnString
  , parseHosts
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Network.Socket (HostName, PortNumber)

-- | TLS connection mode, matching PostgreSQL's @sslmode@ parameter.
data TlsMode
  = TlsDisable       -- ^ No SSL
  | TlsPrefer        -- ^ Try SSL, fall back to plain
  | TlsRequire       -- ^ Require SSL, don't verify server cert
  | TlsVerifyCa      -- ^ Require SSL, verify server cert against CA
  | TlsVerifyFull    -- ^ Require SSL, verify cert + hostname match
  deriving stock (Show, Eq)

-- | Session attribute requirements for multi-host failover.
data TargetSessionAttrs
  = SessionAny            -- ^ Connect to any server
  | SessionReadWrite      -- ^ Must be a read-write server (primary)
  | SessionReadOnly       -- ^ Must be a read-only server (standby)
  | SessionPrimary        -- ^ Must be the primary
  | SessionStandby        -- ^ Must be a standby
  | SessionPreferStandby  -- ^ Prefer standby, fall back to primary
  deriving stock (Show, Eq)

-- | Connection configuration for a single PostgreSQL server.
data ConnConfig = ConnConfig
  { ccHost :: HostName
  -- ^ Primary host. For multi-host, use comma-separated: @\"host1,host2\"@.
  , ccPort :: PortNumber
  , ccDatabase :: ByteString
  , ccUser :: ByteString
  , ccPassword :: ByteString
  , ccTls :: TlsMode
  , ccAppName :: ByteString
  , ccConnectTimeout :: NominalDiffTime
  -- ^ Timeout for establishing the TCP connection (seconds). 0 = no timeout.
  , ccQueryTimeout :: NominalDiffTime
  -- ^ Default timeout for query execution (seconds). 0 = no timeout.
  -- Can be overridden per-query with 'withQueryTimeout'.
  , ccSslCert :: Maybe FilePath
  -- ^ Path to client SSL certificate file (@sslcert@).
  , ccSslKey :: Maybe FilePath
  -- ^ Path to client SSL private key file (@sslkey@).
  , ccSslRootCert :: Maybe FilePath
  -- ^ Path to SSL certificate authority (CA) file (@sslrootcert@).
  -- If @\"system\"@, uses the system certificate store.
  , ccKeepalives :: Bool
  -- ^ Enable TCP keepalives (default: True).
  , ccKeepalivesIdle :: Int
  -- ^ Seconds before first keepalive probe (default: 0 = system default).
  , ccKeepalivesInterval :: Int
  -- ^ Seconds between keepalive probes (default: 0 = system default).
  , ccKeepalivesCount :: Int
  -- ^ Number of failed probes before disconnect (default: 0 = system default).
  , ccTargetSessionAttrs :: TargetSessionAttrs
  -- ^ Required session attributes for multi-host failover (default: 'SessionAny').
  , ccClientEncoding :: ByteString
  -- ^ Client encoding (default: @\"UTF8\"@). Set to @\"auto\"@ for auto-detection.
  , ccOptions :: ByteString
  -- ^ Extra command-line options to send to the server at startup.
  , ccLoadBalanceHosts :: Bool
  -- ^ Randomize host order for multi-host connections (default: False).
  }
  deriving stock (Show)

-- | Default connection configuration: @localhost:5432@, no TLS, 10s connect timeout.
defaultConnConfig :: ConnConfig
defaultConnConfig =
  ConnConfig
    { ccHost = "localhost"
    , ccPort = 5432
    , ccDatabase = ""
    , ccUser = ""
    , ccPassword = ""
    , ccTls = TlsDisable
    , ccAppName = "pg-wire"
    , ccConnectTimeout = 10
    , ccQueryTimeout = 0
    , ccSslCert = Nothing
    , ccSslKey = Nothing
    , ccSslRootCert = Nothing
    , ccKeepalives = True
    , ccKeepalivesIdle = 0
    , ccKeepalivesInterval = 0
    , ccKeepalivesCount = 0
    , ccTargetSessionAttrs = SessionAny
    , ccClientEncoding = "UTF8"
    , ccOptions = ""
    , ccLoadBalanceHosts = False
    }

-- | Parse a PostgreSQL connection string.
-- Supports both URI format (@postgres://user:pass\@host:port/db@)
-- and key=value format (@host=localhost port=5432 dbname=mydb@).
parseConnString :: ByteString -> Either String ConnConfig
parseConnString bs
  | "postgres://" `BS8.isPrefixOf` bs || "postgresql://" `BS8.isPrefixOf` bs =
      parseUri bs
  | otherwise =
      parseKeyValue bs

parseUri :: ByteString -> Either String ConnConfig
parseUri bs = do
  -- Strip scheme: "postgres://..." -> drop up to first '/', then drop "//"
  let afterScheme = BS8.drop 2 (BS8.dropWhile (/= '/') bs) -- drop "scheme://"
      afterSlash = afterScheme

  -- Split user:pass@host:port/db?params
  let (authHost, pathQuery) = case BS8.break (== '/') afterSlash of
        (ah, pq) -> (ah, BS8.drop 1 pq)
      (dbAndParams) = pathQuery
      (db, queryStr) = BS8.break (== '?') dbAndParams

  -- Split auth@host
  let (auth, hostPort) = case BS8.breakEnd (== '@') authHost of
        (a, _) | BS8.null a -> ("", authHost)
        (a, hp) -> (BS8.init a, hp) -- drop trailing '@'

  -- Split user:pass
  let (user, pass) = case BS8.break (== ':') auth of
        (u, p)
          | BS8.null p -> (u, "")
          | otherwise -> (u, BS8.drop 1 p)

  -- Split host:port
  let (host, portStr) = case BS8.break (== ':') hostPort of
        (h, p)
          | BS8.null p -> (h, "5432")
          | otherwise -> (h, BS8.drop 1 p)
      port = maybe 5432 fromIntegral (readPort portStr)

  -- Parse query params for sslmode
  let params = parseQueryParams (BS8.drop 1 queryStr) -- drop '?'
      tlsMode = case lookup "sslmode" params of
        Just "require"     -> TlsRequire
        Just "prefer"      -> TlsPrefer
        Just "disable"     -> TlsDisable
        Just "verify-ca"   -> TlsVerifyCa
        Just "verify-full" -> TlsVerifyFull
        _ -> TlsDisable
      sslCert = BS8.unpack <$> lookup "sslcert" params
      sslKey = BS8.unpack <$> lookup "sslkey" params
      sslRootCert = BS8.unpack <$> lookup "sslrootcert" params

  Right
    ConnConfig
      { ccHost = BS8.unpack host
      , ccPort = port
      , ccDatabase = db
      , ccUser = user
      , ccPassword = pass
      , ccTls = tlsMode
      , ccAppName = "pg-wire"
      , ccConnectTimeout = readTimeout (lookup "connect_timeout" params)
      , ccQueryTimeout = 0
      , ccSslCert = sslCert
      , ccSslKey = sslKey
      , ccSslRootCert = sslRootCert
      , ccKeepalives = True
      , ccKeepalivesIdle = 0
      , ccKeepalivesInterval = 0
      , ccKeepalivesCount = 0
      , ccTargetSessionAttrs = maybe SessionAny parseSessionAttrs
          (lookup "target_session_attrs" params)
      , ccClientEncoding = maybe "UTF8" id (lookup "client_encoding" params)
      , ccOptions = maybe "" id (lookup "options" params)
      , ccLoadBalanceHosts = lookup "load_balance_hosts" params == Just "random"
      }

readTimeout :: Maybe ByteString -> NominalDiffTime
readTimeout Nothing = 10
readTimeout (Just bs) = case BS8.readInt bs of
  Just (n, _) | n > 0 -> fromIntegral n
  _ -> 10

parseKeyValue :: ByteString -> Either String ConnConfig
parseKeyValue bs =
  let pairs = map parsePair (BS8.words bs)
      get key def = maybe def id (lookup key pairs)
      portStr = get "port" "5432"
      port = maybe 5432 fromIntegral (readPort portStr)
      tlsMode = case get "sslmode" "disable" of
        "require"     -> TlsRequire
        "prefer"      -> TlsPrefer
        "verify-ca"   -> TlsVerifyCa
        "verify-full" -> TlsVerifyFull
        _ -> TlsDisable
      getMaybe key = let v = get key "" in if BS8.null v then Nothing else Just (BS8.unpack v)
   in Right
        ConnConfig
          { ccHost = BS8.unpack (get "host" "localhost")
          , ccPort = port
          , ccDatabase = get "dbname" (get "database" "")
          , ccUser = get "user" ""
          , ccPassword = get "password" ""
          , ccTls = tlsMode
          , ccAppName = get "application_name" "pg-wire"
          , ccConnectTimeout = case BS8.readInt (get "connect_timeout" "10") of
              Just (n, _) | n > 0 -> fromIntegral n
              _ -> 10
          , ccQueryTimeout = case BS8.readInt (get "query_timeout" "0") of
              Just (n, _) | n > 0 -> fromIntegral n
              _ -> 0
          , ccSslCert = getMaybe "sslcert"
          , ccSslKey = getMaybe "sslkey"
          , ccSslRootCert = getMaybe "sslrootcert"
          , ccKeepalives = get "keepalives" "1" /= "0"
          , ccKeepalivesIdle = readIntDef 0 (get "keepalives_idle" "0")
          , ccKeepalivesInterval = readIntDef 0 (get "keepalives_interval" "0")
          , ccKeepalivesCount = readIntDef 0 (get "keepalives_count" "0")
          , ccTargetSessionAttrs = parseSessionAttrs (get "target_session_attrs" "any")
          , ccClientEncoding = get "client_encoding" "UTF8"
          , ccOptions = get "options" ""
          , ccLoadBalanceHosts = get "load_balance_hosts" "disable" == "random"
          }
  where
    parsePair p =
      let (k, v) = BS8.break (== '=') p
       in (k, BS8.drop 1 v)

readIntDef :: Int -> ByteString -> Int
readIntDef d bs = case BS8.readInt bs of
  Just (n, _) -> n
  Nothing -> d

readPort :: ByteString -> Maybe Word16
readPort bs
  | BS8.null bs = Nothing
  | BS8.all (\c -> c >= '0' && c <= '9') bs =
      let n = BS8.foldl' (\acc c -> acc * 10 + fromIntegral (fromEnum c - 48)) 0 bs :: Int
       in if n > 0 && n <= 65535 then Just (fromIntegral n) else Nothing
  | otherwise = Nothing

parseQueryParams :: ByteString -> [(ByteString, ByteString)]
parseQueryParams bs
  | BS8.null bs = []
  | otherwise =
      [ (k, BS8.drop 1 v)
      | param <- BS8.split '&' bs
      , let (k, v) = BS8.break (== '=') param
      , not (BS8.null k)
      ]

parseSessionAttrs :: ByteString -> TargetSessionAttrs
parseSessionAttrs "any"              = SessionAny
parseSessionAttrs "read-write"       = SessionReadWrite
parseSessionAttrs "read-only"        = SessionReadOnly
parseSessionAttrs "primary"          = SessionPrimary
parseSessionAttrs "standby"          = SessionStandby
parseSessionAttrs "prefer-standby"   = SessionPreferStandby
parseSessionAttrs _                  = SessionAny

-- | Parse a comma-separated host string into individual hosts.
-- @\"host1,host2,host3\"@ → @[\"host1\", \"host2\", \"host3\"]@
parseHosts :: String -> [String]
parseHosts [] = ["localhost"]
parseHosts s = go s
  where
    go [] = []
    go str = let (h, rest) = break (== ',') str
              in h : case rest of
                       [] -> []
                       (_ : rs) -> go rs
