-- | Utterly unsafe internals of the "Database.Sqlite" module. Useful for
-- people who want access to the SQLite database pointer to manually call
-- SQLite API functions via the FFI.
--
-- Types and functions in this module are *NOT* covered by the PVP and may
-- change breakingly in any future version of the package.
module Database.Sqlite.Internal where

import Data.IORef (IORef)
import Foreign.Ptr (Ptr)

-- | SQLite connection type, consist of an IORef tracking whether the
-- connection has been closed and the raw SQLite C API pointer, wrapped in a
-- 'Connection\'' newtype.
--
-- @since 2.10.2
data Connection = Connection !(IORef Bool) Connection'

-- | Newtype wrapping SQLite C API pointer for a database connection.
--
-- @since 2.10.2
newtype Connection' = Connection' (Ptr ())

-- | Newtype wrapping SQLite C API pointer for a prepared statement.
--
-- @since 2.10.2
newtype Statement = Statement (Ptr ())
