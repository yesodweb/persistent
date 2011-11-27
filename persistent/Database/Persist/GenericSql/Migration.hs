module Database.Persist.GenericSql.Migration
  (   Migration
    , parseMigration
    , parseMigration'
    , printMigration
    , getMigration
    , runMigration
    , runMigrationSilent
    , runMigrationUnsafe
    , migrate
    , commit
    , rollback
  ) where


import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R
import Database.Persist.Store
import Database.Persist.GenericSql.Raw (SqlPersist (..))
import Control.Monad.IO.Control (MonadControlIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad (liftM, unless)
import Data.Text (Text, unpack, snoc)
import qualified Data.Text.IO
import System.IO

type Sql = Text

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

-- Bool indicates if the Sql is safe
type CautiousMigration = [(Bool, Sql)]
allSql :: CautiousMigration -> [Sql]
allSql = map snd
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

type Migration m = WriterT [Text] (WriterT CautiousMigration m) ()

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

printMigration :: MonadControlIO m => Migration (SqlPersist m) -> SqlPersist m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: MonadControlIO m => Migration (SqlPersist m) -> SqlPersist m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadControlIO m
             => Migration (SqlPersist m)
             -> SqlPersist m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: MonadControlIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m [Text]
runMigrationSilent m = runMigration' m True

runMigration' :: MonadControlIO m
              => Migration (SqlPersist m)
              -> Bool -- ^ is silent?
              -> SqlPersist m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ unpack s ++ ";") $ errs
            ]

runMigrationUnsafe :: MonadControlIO m
                   => Migration (SqlPersist m)
                   -> SqlPersist m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> SqlPersist m Text
executeMigrate silent s = do
    unless silent $ liftIO $ hPutStrLn stderr $ "Migrating: " ++ unpack s
    execute' s []
    return s

migrate :: (MonadControlIO m, PersistEntity val)
        => val
        -> Migration (SqlPersist m)
migrate val = do
    conn <- lift $ lift $ SqlPersist ask
    let getter = R.getStmt' conn
    res <- liftIO $ migrateSql conn getter val
    either tell (lift . tell) res

-- | Perform a database commit.
commit :: MonadIO m => SqlPersist m ()
commit = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ commitC conn getter >> begin conn getter

-- | Perform a database rollback.
rollback :: MonadIO m => SqlPersist m ()
rollback = do
    conn <- SqlPersist ask
    let getter = R.getStmt' conn
    liftIO $ rollbackC conn getter >> begin conn getter
