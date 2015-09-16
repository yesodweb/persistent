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

parseMigration :: MonadIO m => Migration -> ReaderT SqlBackend m (Either [Text] CautiousMigration)
parseMigration =
    liftIOReader . liftM go . runWriterT . execWriterT
  where
    go ([], sql) = Right sql
    go (errs, _) = Left errs

    liftIOReader (ReaderT m) = ReaderT $ liftIO . m

-- like parseMigration, but call error or return the CautiousMigration
parseMigration' :: MonadIO m => Migration -> ReaderT SqlBackend m (CautiousMigration)
parseMigration' m = do
  x <- parseMigration m
  case x of
      Left errs -> error $ unlines $ map unpack errs
      Right sql -> return sql

printMigration :: MonadIO m => Migration -> ReaderT SqlBackend m ()
printMigration m = showMigration m
               >>= mapM_ (liftIO . Data.Text.IO.putStrLn)

showMigration :: MonadIO m => Migration -> ReaderT SqlBackend m [Text]
showMigration m = map (flip snoc ';') `liftM` getMigration m

getMigration :: MonadIO m => Migration -> ReaderT SqlBackend m [Sql]
getMigration m = do
  mig <- parseMigration' m
  return $ allSql mig

runMigration :: MonadIO m
             => Migration
             -> ReaderT SqlBackend m ()
runMigration m = runMigration' m False >> return ()

-- | Same as 'runMigration', but returns a list of the SQL commands executed
-- instead of printing them to stderr.
runMigrationSilent :: (MonadBaseControl IO m, MonadIO m)
                   => Migration
                   -> ReaderT SqlBackend m [Text]
runMigrationSilent m = liftBaseOp_ (hSilence [stderr]) $ runMigration' m True

runMigration'
    :: MonadIO m
    => Migration
    -> Bool -- ^ is silent?
    -> ReaderT SqlBackend m [Text]
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

runMigrationUnsafe :: MonadIO m
                   => Migration
                   -> ReaderT SqlBackend m ()
runMigrationUnsafe m = do
    mig <- parseMigration' m
    mapM_ (executeMigrate False) $ sortMigrations $ allSql mig

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

migrate :: [EntityDef]
        -> EntityDef
        -> Migration
migrate allDefs val = do
    conn <- lift $ lift ask
    res <- liftIO $ connMigrateSql conn allDefs (getStmtConn conn) val
    either tell (lift . tell) res
