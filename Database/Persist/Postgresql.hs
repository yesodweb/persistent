{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Postgresql
    ( PostgresqlReader
    , Connection
    , runPostgresql
    , persistPostgresql
    , Int64
    , module Database.Persist.Helper
    , persist
    , connectPostgreSQL
    ) where

import Database.Persist (PersistEntity, Key, Order, Filter, Update,
                         Unique, SqlType (..), PersistValue (..),
                         PersistField (..))
import qualified Database.Persist as P
import Database.Persist.Helper
import Database.Persist.GenericSql
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (intercalate)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad (unless)
import qualified Database.HDBC as H
import Database.HDBC.PostgreSQL
import Data.Int (Int64)
import Database.Persist.Quasi
import Data.Char (toLower)
import Control.Arrow (first, second)

persistPostgresql :: String -> [EntityDef] -> Q [Dec]
persistPostgresql i = fmap concat . mapM (derivePersistPostgresqlReader i)

type PostgresqlReader = ReaderT Connection

runPostgresql :: MonadCatchIO m => PostgresqlReader m a -> Connection -> m a
runPostgresql r conn = do
    res <- onException (runReaderT r conn) $ liftIO (H.rollback conn)
    liftIO $ H.commit conn
    return res

withStmt :: MonadIO m
         => String
         -> [PersistValue]
         -> (RowPopper (PostgresqlReader m) -> PostgresqlReader m a)
         -> PostgresqlReader m a
withStmt sql vals f = do
    conn <- ask
    stmt <- liftIO $ H.prepare conn sql
    _ <- liftIO $ H.execute stmt $ map pToSql vals
    f $ liftIO $ (fmap . fmap) (map pFromSql) $ H.fetchRow stmt

execute sql vals = do
    conn <- ask
    stmt <- liftIO $ H.prepare conn sql
    _ <- liftIO $ H.execute stmt $ map pToSql vals
    return ()

insert :: MonadIO m
       => String -> [String] -> [PersistValue] -> PostgresqlReader m Int64
insert t cols vals = do
    let sql = "INSERT INTO " ++ t ++
              "(" ++ intercalate "," cols ++
              ") VALUES(" ++
              intercalate "," (map (const "?") cols) ++ ") " ++
              "RETURNING id"
    withStmt sql vals $ \pop -> do
        Just [PersistInt64 i] <- pop
        return i

tableExists :: MonadIO m => String -> PostgresqlReader m Bool
tableExists t = do
    conn <- ask
    tables <- liftIO $ H.getTables conn
    return $ map toLower t `elem` tables

derivePersistPostgresqlReader :: String -> EntityDef -> Q [Dec]
derivePersistPostgresqlReader inner t = do
    let wrap = ConT ''ReaderT `AppT` ConT ''Connection
    gs <- [|GenericSql withStmt execute insert tableExists "SERIAL"|]
    deriveGenericSql wrap inner ''MonadIO gs t

pToSql :: PersistValue -> H.SqlValue
pToSql (PersistString s) = H.SqlString s
pToSql (PersistByteString bs) = H.SqlByteString bs
pToSql (PersistInt64 i) = H.SqlInt64 i
pToSql (PersistDouble d) = H.SqlDouble d
pToSql (PersistBool b) = H.SqlBool b
pToSql (PersistDay d) = H.SqlLocalDate d
pToSql (PersistTimeOfDay t) = H.SqlLocalTimeOfDay t
pToSql (PersistUTCTime t) = H.SqlUTCTime t
pToSql PersistNull = H.SqlNull

pFromSql :: H.SqlValue -> PersistValue
pFromSql (H.SqlString s) = PersistString s
pFromSql (H.SqlByteString bs) = PersistByteString bs
pFromSql (H.SqlWord32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlWord64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt32 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInt64 i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlInteger i) = PersistInt64 $ fromIntegral i
pFromSql (H.SqlChar c) = PersistInt64 $ fromIntegral $ fromEnum c
pFromSql (H.SqlBool b) = PersistBool b
pFromSql (H.SqlDouble b) = PersistDouble b
pFromSql (H.SqlRational b) = PersistDouble $ fromRational b
pFromSql (H.SqlLocalDate d) = PersistDay d
pFromSql (H.SqlLocalTimeOfDay d) = PersistTimeOfDay d
pFromSql (H.SqlUTCTime d) = PersistUTCTime d
pFromSql H.SqlNull = PersistNull
pFromSql x = PersistString $ H.fromSql x -- FIXME
