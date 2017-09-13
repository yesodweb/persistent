{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sql.Migration
  ( parseMigration
  , parseMigration'
  , printMigration
  , showMigration
  , getMigration
  , runMigration
  , runMigrationSilent
  , runMigrationUnsafe
  , migrate
  ) where


import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad (liftM, unless)
import Data.Text (Text, unpack, snoc, isPrefixOf, pack)
import qualified Data.Text.IO
import System.IO
import System.IO.Silently (hSilence)
import Control.Monad.Trans.Control (liftBaseOp_)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Types

allSql :: CautiousMigration -> [Sql]
allSql = map snd
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

-- | Given a 'Migration', this parses it and returns either a list of
-- errors associated with the migration or a list of migrations to do.
parseMigration :: MonadIO m => Migration db -> ReaderT (SqlBackend db) m (Either [Text] CautiousMigration)
parseMigration =
    liftIOReader . liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

    liftIOReader (ReaderT m) = ReaderT $ liftIO . m

-- | Like 'parseMigration', but instead of returning the value in an
-- 'Either' value, it calls 'error' on the error values.
parseMigration' :: MonadIO m => Migration db -> ReaderT (SqlBackend db) m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

-- | Prints a migration.
printMigration :: MonadIO m => Migration db -> ReaderT (SqlBackend db) m ()
printMigration m = showMigration m
               >>= mapM_ (liftIO . Data.Text.IO.putStrLn)

-- | Convert a 'Migration' to a list of 'Text' values corresponding to their
-- 'Sql' statements.
showMigration :: MonadIO m => Migration db -> ReaderT (SqlBackend db) m [Text]
showMigration m = map (flip snoc ';') `liftM` getMigration m

-- | Return all of the 'Sql' values associated with the given migration.
-- Calls 'error' if there's a parse error on any migration.
getMigration :: MonadIO m => Migration db -> ReaderT (SqlBackend db) m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

-- | Runs a migration. If the migration fails to parse or if any of the
-- migrations are unsafe, then this calls 'error' to halt the program.
runMigration :: MonadIO m
             => Migration db
             -> ReaderT (SqlBackend db) m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MonadBaseControl IO m, MonadIO m)
                   => Migration db
                   -> ReaderT (SqlBackend db) m [Text]
runMigrationSilent m = liftBaseOp_ (hSilence [stderr]) $ runMigration' m True

-- | Run the given migration against the database. If the migration fails
-- to parse, or there are any unsafe migrations, then this will error at
-- runtime. This returns a list of the migrations that were executed.
runMigration'
    :: MonadIO m
    => Migration db
    -> Bool -- ^ is silent?
    -> ReaderT (SqlBackend db) m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    if any fst mig
        then error $ concat
                 [ "\n\nDatabase migration: manual intervention required.\n"
                 , "The unsafe actions are prefixed by '***' below:\n\n"
                 , unlines $ map displayMigration mig
                 ]
        else mapM (executeMigrate silent) $ sortMigrations $ safeSql mig
  where
    displayMigration :: (Bool, Sql) -> String
    displayMigration (True,  s) = "*** " ++ unpack s ++ ";"
    displayMigration (False, s) = "    " ++ unpack s ++ ";"

-- | Like 'runMigration', but this will perform the unsafe database
-- migrations instead of erroring out.
runMigrationUnsafe :: MonadIO m
                   => Migration db
                   -> ReaderT (SqlBackend db) m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ sortMigrations $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> ReaderT (SqlBackend db) m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ unpack s
    rawExecute s []
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

-- |  Given a list of old entity definitions and a new 'EntityDef' in
-- @val@, this creates a 'Migration' to update the old list of definitions
-- with the new one.
migrate :: [EntityDef]
        -> EntityDef
        -> Migration db
migrate allDefs val = do
    conn <- lift $ lift ask
    res <- liftIO $ connMigrateSql conn allDefs (getStmtConn conn) val
    either tell (lift . tell) res
