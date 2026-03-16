module PgWire.Connection.Config
  ( ConnConfig (..)
  , TlsMode (..)
  , defaultConnConfig
  , parseConnString
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Network.Socket (HostName, PortNumber)

-- | TLS connection mode.
data TlsMode = TlsDisable | TlsPrefer | TlsRequire
  deriving stock (Show, Eq)

-- | Connection configuration.
data ConnConfig = ConnConfig
  { ccHost :: HostName
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
  }
  deriving stock (Show)

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
        Just "require" -> TlsRequire
        Just "prefer" -> TlsPrefer
        Just "disable" -> TlsDisable
        _ -> TlsDisable

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
        "require" -> TlsRequire
        "prefer" -> TlsPrefer
        _ -> TlsDisable
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
          }
  where
    parsePair p =
      let (k, v) = BS8.break (== '=') p
       in (k, BS8.drop 1 v)

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
