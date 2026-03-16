module PgWire.Pool.Config
  ( PoolConfig (..)
  , defaultPoolConfig
  ) where

import Data.ByteString (ByteString)
import Data.Time (NominalDiffTime)

-- | Configuration for a connection pool.
data PoolConfig = PoolConfig
  { poolConnString :: ByteString
  , poolSize :: Int
  , poolIdleTime :: NominalDiffTime
  , poolMaxLife :: NominalDiffTime
  , poolAcquireTimeout :: NominalDiffTime
  }
  deriving stock (Show)

defaultPoolConfig :: PoolConfig
defaultPoolConfig =
  PoolConfig
    { poolConnString = ""
    , poolSize = 10
    , poolIdleTime = 600
    , poolMaxLife = 3600
    , poolAcquireTimeout = 10
    }
