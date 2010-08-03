{-# LANGUAGE ForeignFunctionInterface #-}
module Database.Mysql
    ( MysqlSettings (..)
    , open
    , close
    , Connection
    , prepare
    , finalize
    ) where

import Foreign.C
import Foreign.Ptr

data MysqlSettings = MysqlSettings
    { mysqlHost :: String
    , mysqlUser :: String
    , mysqlPass :: String
    , mysqlDb   :: String
    , mysqlPort :: Int
    }

newtype Connection = Connection (Ptr ())
newtype Statement = Statement (Ptr ())

foreign import ccall "mysql_init"
    c'init :: Ptr () -> IO Connection
foreign import ccall "mysql_real_connect"
    c'open :: Connection -> Ptr CChar -> Ptr CChar -> Ptr CChar
           -> Ptr CChar -> CInt -> Ptr CChar -> CLong -> IO (Ptr ())

getError :: Connection -> String -> IO String
getError _ _ = error "An error occurred"

tellError :: Connection -> String -> IO a
tellError conn str = do
    err <- getError conn str
    error err

open :: MysqlSettings -> IO Connection
open (MysqlSettings host' user' pass' db' port') =
    withCString host' $ \host ->
    withCString user' $ \user ->
    withCString pass' $ \pass ->
    withCString db'   $ \db   -> do
        let port = fromIntegral port'
        conn <- c'init nullPtr
        conn' <- c'open conn host user pass db port nullPtr 0
        if conn' == nullPtr
            then do
                err <- getError conn "open"
                close conn
                error err
            else return conn

foreign import ccall "mysql_close"
    close :: Connection -> IO ()

foreign import ccall "mysql_stmt_init"
    c'stmtInit :: Connection -> IO Statement
foreign import ccall "mysql_stmt_prepare"
    c'stmtPrepare :: Statement -> Ptr CChar -> CLong -> IO CInt
foreign import ccall "mysql_stmt_close"
    c'stmtClose :: Statement -> IO CInt
prepare :: Connection -> String -> IO Statement
prepare conn sql' = withCStringLen sql' $ \(sql, len') -> do
    let len = fromIntegral len'
    stmt <- c'stmtInit conn
    res <- c'stmtPrepare stmt sql len
    if res == 0
        then return stmt
        else do
            _ <- c'stmtClose stmt
            tellError conn $ "prepare " ++ sql'
finalize :: Statement -> IO ()
finalize stmt = c'stmtClose stmt >> return ()
