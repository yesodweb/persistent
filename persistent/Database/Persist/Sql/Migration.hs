{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Sql.Migration
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
unsafeSql :: CautiousMigration -> [Sql]
unsafeSql = allSql . filter fst
safeSql :: CautiousMigration -> [Sql]
safeSql = allSql . filter (not . fst)

parseMigration :: MonadIO m => Migration -> ReaderT Connection m (Either [Text] CautiousMigration)
parseMigration =
    liftIOReader . liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

    liftIOReader (ReaderT m) = ReaderT $ liftIO . m

-- like parseMigration, but call error or return the CautiousMigration
parseMigration' :: MonadIO m => Migration -> ReaderT Connection m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

printMigration :: MonadIO m => Migration -> ReaderT Connection m ()
printMigration m = do
  mig <- parseMigration' m
  mapM_ (liftIO . Data.Text.IO.putStrLn . flip snoc ';') (allSql mig)

getMigration :: (MonadBaseControl IO m, MonadIO m) => Migration -> ReaderT Connection m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadIO m
             => Migration
             -> ReaderT Connection m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MonadBaseControl IO m, MonadIO m)
                   => Migration
                   -> ReaderT Connection m [Text]
runMigrationSilent m = liftBaseOp_ (hSilence [stderr]) $ runMigration' m True

runMigration'
    :: MonadIO m
    => Migration
    -> Bool -- ^ is silent?
    -> ReaderT Connection m [Text]
runMigration' m silent = do
    mig <- parseMigration' m
    case unsafeSql mig of
        []   -> mapM (executeMigrate silent) $ sortMigrations $ safeSql mig
        errs -> error $ concat
            [ "\n\nDatabase migration: manual intervention required.\n"
            , "The following actions are considered unsafe:\n\n"
            , unlines $ map (\s -> "    " ++ unpack s ++ ";") $ errs
            ]

runMigrationUnsafe :: MonadIO m
                   => Migration
                   -> ReaderT Connection m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ sortMigrations $ allSql mig

executeMigrate :: MonadIO m => Bool -> Text -> ReaderT Connection m Text
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

migrate :: [EntityDef]
        -> EntityDef
        -> Migration
migrate allDefs val = do
    conn <- lift $ lift ask
    res <- liftIO $ connMigrateSql conn allDefs (getStmtConn conn) val
    either tell (lift . tell) res
