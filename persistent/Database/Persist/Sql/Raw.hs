{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Sql.Raw where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
import Data.IORef (writeIORef, readIORef, newIORef)
import Control.Exception (throwIO)
import Control.Monad (when, liftM)
import Data.Text (Text, pack)
import Control.Monad.Logger (logDebugS)
import Data.Int (Int64)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Conduit

rawQuery :: (MonadSqlPersist m, MonadResource m)
         => Text
         -> [PersistValue]
         -> Source m [PersistValue]
rawQuery sql vals = do
    lift $ $logDebugS (pack "SQL") $ pack $ show sql ++ " " ++ show vals
    conn <- lift askSqlConn
    bracketP
        (getStmtConn conn sql)
        stmtReset
        (flip stmtQuery vals)

rawExecute :: MonadSqlPersist m => Text -> [PersistValue] -> m ()
rawExecute x y = liftM (const ()) $ rawExecuteCount x y

rawExecuteCount :: MonadSqlPersist m => Text -> [PersistValue] -> m Int64
rawExecuteCount sql vals = do
    $logDebugS (pack "SQL") $ pack $ show sql ++ " " ++ show vals
    stmt <- getStmt sql
    res <- liftIO $ stmtExecute stmt vals
    liftIO $ stmtReset stmt
    return res

getStmt :: MonadSqlPersist m => Text -> m Statement
getStmt sql = do
    conn <- askSqlConn
    liftIO $ getStmtConn conn sql

getStmtConn :: Connection -> Text -> IO Statement
getStmtConn conn sql = do
    smap <- liftIO $ readIORef $ connStmtMap conn
    case Map.lookup sql smap of
        Just stmt -> return stmt
        Nothing -> do
            stmt' <- liftIO $ connPrepare conn sql
            iactive <- liftIO $ newIORef True
            let stmt = Statement
                    { stmtFinalize = do
                        active <- readIORef iactive
                        if active
                            then do
                                stmtFinalize stmt'
                                writeIORef iactive False
                            else return ()
                    , stmtReset = do
                        active <- readIORef iactive
                        when active $ stmtReset stmt'
                    , stmtExecute = \x -> do
                        active <- readIORef iactive
                        if active
                            then stmtExecute stmt' x
                            else throwIO $ StatementAlreadyFinalized sql
                    , stmtQuery = \x -> do
                        active <- liftIO $ readIORef iactive
                        if active
                            then stmtQuery stmt' x
                            else liftIO $ throwIO $ StatementAlreadyFinalized sql
                    }
            liftIO $ writeIORef (connStmtMap conn) $ Map.insert sql stmt smap
            return stmt

-- | Execute a raw SQL statement and return its results as a
-- list.
--
-- If you're using 'Entity'@s@ (which is quite likely), then you
-- /must/ use entity selection placeholders (double question
-- mark, @??@).  These @??@ placeholders are then replaced for
-- the names of the columns that we need for your entities.
-- You'll receive an error if you don't use the placeholders.
-- Please see the 'Entity'@s@ documentation for more details.
--
-- You may put value placeholders (question marks, @?@) in your
-- SQL query.  These placeholders are then replaced by the values
-- you pass on the second parameter, already correctly escaped.
-- You may want to use 'toPersistValue' to help you constructing
-- the placeholder values.
--
-- Since you're giving a raw SQL statement, you don't get any
-- guarantees regarding safety.  If 'rawSql' is not able to parse
-- the results of your query back, then an exception is raised.
-- However, most common problems are mitigated by using the
-- entity selection placeholder @??@, and you shouldn't see any
-- error at all if you're not using 'Single'.
rawSql :: (RawSql a, MonadSqlPersist m, MonadResource m)
       => Text             -- ^ SQL statement, possibly with placeholders.
       -> [PersistValue]   -- ^ Values to fill the placeholders.
       -> m [a]
rawSql stmt = run
    where
      getType :: (x -> m [a]) -> a
      getType = error "rawSql.getType"

      x = getType run
      process = rawSqlProcessRow

      withStmt' colSubsts params = do
            rawQuery sql params
          where
            sql = T.concat $ makeSubsts colSubsts $ T.splitOn placeholder stmt
            placeholder = "??"
            makeSubsts (s:ss) (t:ts) = t : s : makeSubsts ss ts
            makeSubsts []     []     = []
            makeSubsts []     ts     = [T.intercalate placeholder ts]
            makeSubsts ss     []     = error (concat err)
                where
                  err = [ "rawsql: there are still ", show (length ss)
                        , "'??' placeholder substitutions to be made "
                        , "but all '??' placeholders have already been "
                        , "consumed.  Please read 'rawSql's documentation "
                        , "on how '??' placeholders work."
                        ]

      run params = do
        conn <- askSqlConn
        let (colCount, colSubsts) = rawSqlCols (connEscapeName conn) x
        withStmt' colSubsts params $$ firstRow colCount

      firstRow colCount = do
        mrow <- await
        case mrow of
          Nothing -> return []
          Just row
              | colCount == length row -> getter mrow
              | otherwise              -> fail $ concat
                  [ "rawSql: wrong number of columns, got "
                  , show (length row), " but expected ", show colCount
                  , " (", rawSqlColCountReason x, ")." ]

      getter = go id
          where
            go acc Nothing = return (acc [])
            go acc (Just row) =
              case process row of
                Left err -> fail (T.unpack err)
                Right r  -> await >>= go (acc . (r:))
