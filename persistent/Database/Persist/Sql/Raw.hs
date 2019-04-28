module Database.Persist.Sql.Raw where

import Control.Exception (throwIO)
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (logDebugNS, runLoggingT)
import Control.Monad.Reader (ReaderT, ask, MonadReader)
import Control.Monad.Trans.Resource (MonadResource,release)
import Data.Acquire (allocateAcquire, Acquire, mkAcquire, with)
import Data.Conduit
import Data.IORef (writeIORef, readIORef, newIORef)
import qualified Data.Map as Map
import Data.Int (Int64)
import Data.Text (Text, pack)
import qualified Data.Text as T

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class

rawQuery :: (MonadResource m, MonadReader env m, BackendCompatible SqlBackend env)
         => Text
         -> [PersistValue]
         -> ConduitM () [PersistValue] m ()
rawQuery sql vals = do
    srcRes <- liftPersist $ rawQueryRes sql vals
    (releaseKey, src) <- allocateAcquire srcRes
    src
    release releaseKey

rawQueryRes
    :: (MonadIO m1, MonadIO m2, BackendCompatible SqlBackend env)
    => Text
    -> [PersistValue]
    -> ReaderT env m1 (Acquire (ConduitM () [PersistValue] m2 ()))
rawQueryRes sql vals = do
    conn <- projectBackend `liftM` ask
    let make = do
            runLoggingT (logDebugNS (pack "SQL") $ T.append sql $ pack $ "; " ++ show vals)
                (connLogFunc conn)
            getStmtConn conn sql
    return $ do
        stmt <- mkAcquire make stmtReset
        stmtQuery stmt vals

-- | Execute a raw SQL statement
rawExecute :: (MonadIO m, BackendCompatible SqlBackend backend)
           => Text            -- ^ SQL statement, possibly with placeholders.
           -> [PersistValue]  -- ^ Values to fill the placeholders.
           -> ReaderT backend m ()
rawExecute x y = liftM (const ()) $ rawExecuteCount x y

-- | Execute a raw SQL statement and return the number of
-- rows it has modified.
rawExecuteCount :: (MonadIO m, BackendCompatible SqlBackend backend)
                => Text            -- ^ SQL statement, possibly with placeholders.
                -> [PersistValue]  -- ^ Values to fill the placeholders.
                -> ReaderT backend m Int64
rawExecuteCount sql vals = do
    conn <- projectBackend `liftM` ask
    runLoggingT (logDebugNS (pack "SQL") $ T.append sql $ pack $ "; " ++ show vals)
        (connLogFunc conn)
    stmt <- getStmt sql
    res <- liftIO $ stmtExecute stmt vals
    liftIO $ stmtReset stmt
    return res

getStmt
  :: (MonadIO m, BackendCompatible SqlBackend backend)
  => Text -> ReaderT backend m Statement
getStmt sql = do
    conn <- projectBackend `liftM` ask
    liftIO $ getStmtConn conn sql

getStmtConn :: SqlBackend -> Text -> IO Statement
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
                        when active $ do stmtFinalize stmt'
                                         writeIORef iactive False
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
-- list. If you do not expect a return value, use of
-- `rawExecute` is recommended.
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
--
-- Some example of 'rawSql' based on this schema:
--
-- @
-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Person
--     name String
--     age Int Maybe
--     deriving Show
-- BlogPost
--     title String
--     authorId PersonId
--     deriving Show
-- |]
-- @
--
-- Examples based on the above schema:
--
-- @
-- getPerson :: MonadIO m => ReaderT SqlBackend m [Entity Person]
-- getPerson = rawSql "select ?? from person where name=?" [PersistText "john"]
--
-- getAge :: MonadIO m => ReaderT SqlBackend m [Single Int]
-- getAge = rawSql "select person.age from person where name=?" [PersistText "john"]
--
-- getAgeName :: MonadIO m => ReaderT SqlBackend m [(Single Int, Single Text)]
-- getAgeName = rawSql "select person.age, person.name from person where name=?" [PersistText "john"]
--
-- getPersonBlog :: MonadIO m => ReaderT SqlBackend m [(Entity Person, Entity BlogPost)]
-- getPersonBlog = rawSql "select ??,?? from person,blog_post where person.id = blog_post.author_id" []
-- @
--
-- Minimal working program for PostgreSQL backend based on the above concepts:
--
-- > {-# LANGUAGE EmptyDataDecls             #-}
-- > {-# LANGUAGE FlexibleContexts           #-}
-- > {-# LANGUAGE GADTs                      #-}
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > {-# LANGUAGE MultiParamTypeClasses      #-}
-- > {-# LANGUAGE OverloadedStrings          #-}
-- > {-# LANGUAGE QuasiQuotes                #-}
-- > {-# LANGUAGE TemplateHaskell            #-}
-- > {-# LANGUAGE TypeFamilies               #-}
-- >
-- > import           Control.Monad.IO.Class  (liftIO)
-- > import           Control.Monad.Logger    (runStderrLoggingT)
-- > import           Database.Persist
-- > import           Control.Monad.Reader
-- > import           Data.Text
-- > import           Database.Persist.Sql
-- > import           Database.Persist.Postgresql
-- > import           Database.Persist.TH
-- >
-- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- > Person
-- >     name String
-- >     age Int Maybe
-- >     deriving Show
-- > |]
-- >
-- > conn = "host=localhost dbname=new_db user=postgres password=postgres port=5432"
-- >
-- > getPerson :: MonadIO m => ReaderT SqlBackend m [Entity Person]
-- > getPerson = rawSql "select ?? from person where name=?" [PersistText "sibi"]
-- >
-- > liftSqlPersistMPool y x = liftIO (runSqlPersistMPool y x)
-- >
-- > main :: IO ()
-- > main = runStderrLoggingT $ withPostgresqlPool conn 10 $ liftSqlPersistMPool $ do
-- >          runMigration migrateAll
-- >          xs <- getPerson
-- >          liftIO (print xs)
-- >

rawSql :: (RawSql a, MonadIO m, BackendCompatible SqlBackend backend)
       => Text             -- ^ SQL statement, possibly with placeholders.
       -> [PersistValue]   -- ^ Values to fill the placeholders.
       -> ReaderT backend m [a]
rawSql stmt = run
    where
      getType :: (x -> m [a]) -> a
      getType = error "rawSql.getType"

      x = getType run
      process = rawSqlProcessRow

      withStmt' colSubsts params sink = do
            srcRes <- rawQueryRes sql params
            liftIO $ with srcRes (\src -> runConduit $ src .| sink)
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
        conn <- projectBackend `liftM` ask
        let (colCount, colSubsts) = rawSqlCols (connEscapeName conn) x
        withStmt' colSubsts params $ firstRow colCount

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
