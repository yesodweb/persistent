{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.GenericSql.Migration
  ( parseMigration
  , parseMigration'
  , printMigration
  , getMigration
  , runMigration
  , runMigrationSilent
  , runMigrationUnsafe
  , migrate
  ) where


import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad (liftM, unless)
import Data.Text (Text, unpack, snoc, isPrefixOf, pack)
import qualified Data.Text.IO
import System.IO
import Control.Monad.Logger (MonadLogger)
import System.IO.Silently (hSilence)
import System.IO (stderr)
import Control.Monad.Trans.Control (liftBaseOp_)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import Database.Persist.Sql.Raw
import Database.Persist.Class
import Database.Persist.Types

execute' :: (MonadIO m, MonadLogger m) => Text -> [PersistValue] -> SqlPersist m () -- FIXME remove
execute' = rawExecute

allSql :: CautiousMigration -> [Sql]
allSql = map snd
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

parseMigration :: Monad m => Migration m -> m (Either [Text] CautiousMigration)
parseMigration =
    liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

-- like parseMigration, but call error or return the CautiousMigration
parseMigration' :: Monad m => Migration m -> m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

printMigration :: (MonadBaseControl IO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: (MonadBaseControl IO m, MonadIO m) => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m [Text]
runMigrationSilent m = liftBaseOp_ (hSilence [stderr]) $ runMigration' m True

runMigration'
    :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
    => Migration (SqlPersist m)
    -> Bool -- ^ is silent?
    -> SqlPersist m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ sortMigrations $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ unpack s ++ ";") $ errs
            ]

runMigrationUnsafe :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ sortMigrations $ allSql mig

executeMigrate :: (MonadIO m, MonadLogger m) => Bool -> Text -> SqlPersist m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ unpack s
    execute' s []
    return s

-- | Sort the alter DB statements so tables are created before constraints are
-- added.
sortMigrations :: [Sql] -> [Sql]
sortMigrations x =
    filter isCreate x ++ filter (not . isCreate) x
  where
    -- Note the use of lower-case e. This (hack) allows backends to explicitly
    -- choose to have this special sorting applied.
    isCreate t = pack "CREATe " `isPrefixOf` t

migrate :: (MonadSqlPersist m, PersistEntity val)
        => [EntityDef]
        -> val
        -> Migration m
migrate allDefs val = do
    conn <- askSqlConn
    res <- liftIO $ connMigrateSql conn allDefs (getStmtConn conn) val
    either tell (lift . tell) res

-- | Perform a database commit.
commit :: (MonadSqlPersist m, MonadIO m) => m ()
commit = do
    conn <- askSqlConn
    liftIO $ do
        connCommit conn $ getStmtConn conn
        connBegin conn $ getStmtConn conn

-- | Perform a database rollback.
rollback :: (MonadSqlPersist m, MonadIO m) => m ()
rollback = do
    conn <- askSqlConn
    liftIO $ do
        connRollback conn $ getStmtConn conn
        connBegin conn $ getStmtConn conn
