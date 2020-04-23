module Database.Persist.Sql.Migration
  ( parseMigration
  , parseMigration'
  , printMigration
  , showMigration
  , getMigration
  , runMigration
  , runMigrationQuiet
  , runMigrationSilent
  , runMigrationUnsafe
  , runMigrationUnsafeQuiet
  , migrate
  -- * Utilities for constructing migrations
  , reportErrors
  , reportError
  , addMigrations
  , addMigration
  ) where


import Control.Exception (throwIO)
import Control.Monad (liftM, unless)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.Writer
import Data.Text (Text, unpack, snoc, isPrefixOf, pack)
import qualified Data.Text.IO
import System.IO
import System.IO.Silently (hSilence)

import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Types
import Database.Persist.Sql.Orphan.PersistStore()

allSql :: CautiousMigration -> [Sql]
allSql = map snd
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

-- | Given a 'Migration', this parses it and returns either a list of
-- errors associated with the migration or a list of migrations to do.
parseMigration :: MonadIO m => Migration -> ReaderT SqlBackend m (Either [Text] CautiousMigration)
parseMigration =
    liftIOReader . liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

    liftIOReader (ReaderT m) = ReaderT $ liftIO . m

-- | Like 'parseMigration', but instead of returning the value in an
-- 'Either' value, it calls 'error' on the error values.
parseMigration' :: MonadIO m => Migration -> ReaderT SqlBackend m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

-- | Prints a migration.
printMigration :: MonadIO m => Migration -> ReaderT SqlBackend m ()
printMigration m = showMigration m
               >>= mapM_ (liftIO . Data.Text.IO.putStrLn)

-- | Convert a 'Migration' to a list of 'Text' values corresponding to their
-- 'Sql' statements.
showMigration :: MonadIO m => Migration -> ReaderT SqlBackend m [Text]
showMigration m = map (flip snoc ';') `liftM` getMigration m

-- | Return all of the 'Sql' values associated with the given migration.
-- Calls 'error' if there's a parse error on any migration.
getMigration :: MonadIO m => Migration -> ReaderT SqlBackend m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

-- | Runs a migration. If the migration fails to parse or if any of the
-- migrations are unsafe, then this throws a 'PersistUnsafeMigrationException'.
runMigration :: MonadIO m
             => Migration
             -> ReaderT SqlBackend m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but does not report the individual migrations on
-- stderr. Instead it returns a list of the executed SQL commands.
--
-- This is a safer/more robust alternative to 'runMigrationSilent', but may be
-- less silent for some persistent implementations, most notably
-- persistent-postgresql
--
-- @since 2.10.2
runMigrationQuiet :: MonadIO m
                  => Migration
                  -> ReaderT SqlBackend m [Text]
runMigrationQuiet m = runMigration' m True

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
--
-- This function silences the migration by remapping 'stderr'. As a result, it
-- is not thread-safe and can clobber output from other parts of the program.
-- This implementation method was chosen to also silence postgresql migration
-- output on stderr, but is not recommended!
runMigrationSilent :: MonadUnliftIO m
                   => Migration
                   -> ReaderT SqlBackend m [Text]
runMigrationSilent m = withRunInIO $ \run ->
  hSilence [stderr] $ run $ runMigration' m True

-- | Run the given migration against the database. If the migration fails
-- to parse, or there are any unsafe migrations, then this will error at
-- runtime. This returns a list of the migrations that were executed.
runMigration'
    :: MonadIO m
    => Migration
    -> Bool -- ^ is silent?
    -> ReaderT SqlBackend m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    if any fst mig
        then liftIO . throwIO $ PersistUnsafeMigrationException mig
        else mapM (executeMigrate silent) $ sortMigrations $ safeSql mig

-- | Like 'runMigration', but this will perform the unsafe database
-- migrations instead of erroring out.
runMigrationUnsafe :: MonadIO m
                   => Migration
                   -> ReaderT SqlBackend m ()
runMigrationUnsafe m = runMigrationUnsafe' False m >> return ()

-- | Same as 'runMigrationUnsafe', but returns a list of the SQL commands
-- executed instead of printing them to stderr.
--
-- @since 2.10.2
runMigrationUnsafeQuiet :: MonadIO m
                        => Migration
                        -> ReaderT SqlBackend m [Text]
runMigrationUnsafeQuiet = runMigrationUnsafe' True

runMigrationUnsafe' :: MonadIO m
                    => Bool
                    -> Migration
                    -> ReaderT SqlBackend m [Text]
runMigrationUnsafe' silent m = do
    mig <- parseMigration' m
    mapM (executeMigrate silent) $ sortMigrations $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> ReaderT SqlBackend m Text
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
        -> Migration
migrate allDefs val = do
    conn <- lift $ lift ask
    res <- liftIO $ connMigrateSql conn allDefs (getStmtConn conn) val
    either reportErrors addMigrations res

-- | Report a single error in a 'Migration'.
--
-- @since 2.9.2
reportError :: Text -> Migration
reportError = tell . pure

-- | Report multiple errors in a 'Migration'.
--
-- @since 2.9.2
reportErrors :: [Text] -> Migration
reportErrors = tell

-- | Add a migration to the migration plan.
--
-- @since 2.9.2
addMigration
    :: Bool
    -- ^ Is the migration safe to run? (eg a non-destructive and idempotent
    -- update on the schema)
    -> Sql
    -- ^ A 'Text' value representing the command to run on the database.
    -> Migration
addMigration isSafe sql = lift (tell [(isSafe, sql)])

-- | Add a 'CautiousMigration' (aka a @[('Bool', 'Text')]@) to the
-- migration plan.
--
-- @since 2.9.2
addMigrations
    :: CautiousMigration
    -> Migration
addMigrations = lift . tell
