-- | This module documents tools and utilities for running SQL migrations.
--
-- A 'Migration' is (currently) an alias for a 'WriterT' of
module Database.Persist.Sql.Migration
  (
    -- * Types
    Migration
  , CautiousMigration
  , Sql
    -- * Using a 'Migration'
  , showMigration
  , parseMigration
  , parseMigration'
  , printMigration
  , getMigration
  , runMigration
  , runMigrationQuiet
  , runMigrationSilent
  , runMigrationUnsafe
  , runMigrationUnsafeQuiet
  , migrate
  -- * Utilities for constructing migrations
  -- | While 'migrate' is capable of creating a 'Migration' for you, it's not
  -- the only way you can write migrations. You can use these utilities to write
  -- extra steps in your migrations.
  --
  -- As an example, let's say we want to enable the @citext@ extension on
  -- @postgres@ as part of our migrations.
  --
  -- @
  -- 'Database.Persist.TH.share' ['Database.Persist.TH.mkPersist' sqlSettings, 'Database.Persist.TH.mkMigration' "migrateAll"] ...
  --
  -- migration :: 'Migration'
  -- migration = do
  --     'runSqlCommand' $
  --         'rawExecute_' "CREATE EXTENSION IF NOT EXISTS \"citext\";"
  --     migrateAll
  -- @
  --
  -- For raw commands, you can also just write 'addMigration':
  --
  -- @
  -- migration :: 'Migration'
  -- migration = do
  --     'addMigration' "CREATE EXTENSION IF NOT EXISTS \"citext\";"
  --     migrateAll
  -- @
  , reportErrors
  , reportError
  , addMigrations
  , addMigration
  , runSqlCommand
  -- * If something goes wrong...
  , PersistUnsafeMigrationException(..)
  ) where


import Control.Exception (throwIO)
import Control.Monad (liftM, unless)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.Writer
import Data.Text (Text, isPrefixOf, pack, snoc, unpack)
import qualified Data.Text.IO
import GHC.Stack
import System.IO
import System.IO.Silently (hSilence)

import Database.Persist.Sql.Orphan.PersistStore ()
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Types
import Database.Persist.Sql.Types.Internal
import Database.Persist.Types
import Control.Exception (Exception(..))

type Sql = Text

-- | A list of SQL operations, marked with a safety flag. If the 'Bool' is
-- 'True', then the operation is *unsafe* - it might be destructive, or
-- otherwise not idempotent. If the 'Bool' is 'False', then the operation
-- is *safe*, and can be run repeatedly without issues.
type CautiousMigration = [(Bool, Sql)]

-- | A 'Migration' is a four level monad stack consisting of:
--
-- * @'WriterT' ['Text']@ representing a log of errors in the migrations.
-- * @'WriterT' 'CautiousMigration'@ representing a list of migrations to
--   run, along with whether or not they are safe.
-- * @'ReaderT' 'SqlBackend'@, aka the 'SqlPersistT' transformer for
--   database interop.
-- * @'IO'@ for arbitrary IO.
type Migration = WriterT [Text] (WriterT CautiousMigration (ReaderT SqlBackend IO)) ()

allSql :: CautiousMigration -> [Sql]
allSql = map snd

safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

-- | Given a 'Migration', this parses it and returns either a list of
-- errors associated with the migration or a list of migrations to do.
parseMigration :: (HasCallStack, MonadIO m) => Migration -> ReaderT SqlBackend m (Either [Text] CautiousMigration)
parseMigration =
    liftIOReader . liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

    liftIOReader (ReaderT m) = ReaderT $ liftIO . m

-- | Like 'parseMigration', but instead of returning the value in an
-- 'Either' value, it calls 'error' on the error values.
parseMigration' :: (HasCallStack, MonadIO m) => Migration -> ReaderT SqlBackend m CautiousMigration
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

-- | Prints a migration.
printMigration :: (HasCallStack, MonadIO m) => Migration -> ReaderT SqlBackend m ()
printMigration m = showMigration m
               >>= mapM_ (liftIO . Data.Text.IO.putStrLn)

-- | Convert a 'Migration' to a list of 'Text' values corresponding to their
-- 'Sql' statements.
showMigration :: (HasCallStack, MonadIO m) => Migration -> ReaderT SqlBackend m [Text]
showMigration m = map (flip snoc ';') `liftM` getMigration m

-- | Return all of the 'Sql' values associated with the given migration.
-- Calls 'error' if there's a parse error on any migration.
getMigration :: (MonadIO m, HasCallStack) => Migration -> ReaderT SqlBackend m [Sql]
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
    :: (HasCallStack, MonadIO m)
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
runMigrationUnsafeQuiet :: (HasCallStack, MonadIO m)
                        => Migration
                        -> ReaderT SqlBackend m [Text]
runMigrationUnsafeQuiet = runMigrationUnsafe' True

runMigrationUnsafe' :: (HasCallStack, MonadIO m)
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
    -- ^ Is the migration unsafe to run? (eg a destructive or non-idempotent
    -- update on the schema). If 'True', the migration is *unsafe*, and will
    -- need to be run manually later. If 'False', the migration is *safe*, and
    -- can be run any number of times.
    -> Sql
    -- ^ A 'Text' value representing the command to run on the database.
    -> Migration
addMigration isUnsafe sql = lift (tell [(isUnsafe, sql)])

-- | Add a 'CautiousMigration' (aka a @[('Bool', 'Text')]@) to the
-- migration plan.
--
-- @since 2.9.2
addMigrations
    :: CautiousMigration
    -> Migration
addMigrations = lift . tell

-- | Run an action against the database during a migration. Can be useful for eg
-- creating Postgres extensions:
--
-- @
-- runSqlCommand $ 'rawExecute' "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" []
-- @
--
-- @since 2.13.0.0
runSqlCommand :: SqlPersistT IO () -> Migration
runSqlCommand = lift . lift

-- | An exception indicating that Persistent refused to run some unsafe
-- migrations. Contains a list of pairs where the Bool tracks whether the
-- migration was unsafe (True means unsafe), and the Sql is the sql statement
-- for the migration.
--
-- @since 2.11.1.0
newtype PersistUnsafeMigrationException
  = PersistUnsafeMigrationException [(Bool, Sql)]

-- | This 'Show' instance renders an error message suitable for printing to the
-- console. This is a little dodgy, but since GHC uses Show instances when
-- displaying uncaught exceptions, we have little choice.
instance Show PersistUnsafeMigrationException where
  show (PersistUnsafeMigrationException mig) =
    concat
      [ "\n\nDatabase migration: manual intervention required.\n"
      , "The unsafe actions are prefixed by '***' below:\n\n"
      , unlines $ map displayMigration mig
      ]
    where
      displayMigration :: (Bool, Sql) -> String
      displayMigration (True,  s) = "*** " ++ unpack s ++ ";"
      displayMigration (False, s) = "    " ++ unpack s ++ ";"

instance Exception PersistUnsafeMigrationException
