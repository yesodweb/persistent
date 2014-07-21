-- | A MySQL backend for @persistent@.
module Database.Persist.MySQL
  ( withMySQLPool
  , withMySQLConn
  , createMySQLPool
  , module Database.Persist.Sql
  , MySQL.ConnectInfo(..)
  , MySQLBase.SSLInfo(..)
  , MySQL.defaultConnectInfo
  , MySQLBase.defaultSSLInfo
  , MySQLConf(..)
  ) where

import Database.Persist.Sql
import Database.Persist.MySQL.Internal
import qualified Database.MySQL.Simple        as MySQL
import qualified Database.MySQL.Base          as MySQLBase
