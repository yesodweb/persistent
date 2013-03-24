{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
-- | Code that is only needed for writing GenericSql backends.
module Database.Persist.GenericSql.Internal
    ( Connection (..)
    , Statement (..)
    , withSqlConn
    , withSqlPool
    , createSqlPool
    , mkColumns
    , Column (..)
    , logSQL
    , InsertSqlResult (..)
    ) where

import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Quasi
import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.IORef
import Control.Monad.IO.Class
import Data.Conduit.Pool
import Control.Exception.Lifted (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid (Monoid, mappend, mconcat)
import qualified Data.Conduit as C
import Language.Haskell.TH.Syntax (Q, Exp)
import Control.Monad.Logger (logDebugS)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Int (Int64)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class

withSqlPool :: MonadIO m
            => IO Connection -- ^ create a new connection
            -> Int -- ^ connection count
            -> (Pool Connection -> m a)
            -> m a
withSqlPool mkConn connCount f = do
    pool <- createSqlPool mkConn connCount
    f pool

createSqlPool :: MonadIO m
              => IO Connection
              -> Int
              -> m (Pool Connection)
createSqlPool mkConn = liftIO . createPool mkConn close' 1 20

withSqlConn :: (MonadIO m, MonadBaseControl IO m)
            => IO Connection -> (Connection -> m a) -> m a
withSqlConn open = bracket (liftIO open) (liftIO . close')

close' :: Connection -> IO ()
close' conn = do
    readIORef (stmtMap conn) >>= mapM_ finalize . Map.elems
    close conn

resolveTableName :: [EntityDef] -> HaskellName -> DBName
resolveTableName [] (HaskellName hn) = error $ "Table not found: " `mappend` T.unpack hn
resolveTableName (e:es) hn
    | entityHaskell e == hn = entityDB e
    | otherwise = resolveTableName es hn

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => [EntityDef] -> val -> ([Column], [UniqueDef])
mkColumns allDefs val =
    (cols, entityUniques t)
  where
    cols :: [Column]
    cols = zipWith go (entityFields t)
         $ toPersistFields
         $ halfDefined `asTypeOf` val

    t :: EntityDef
    t = entityDef val

    tn :: DBName
    tn = entityDB t

    go :: FieldDef -> SomePersistField -> Column
    go fd p =
        Column
            (fieldDB fd)
            (nullable (fieldAttrs fd) /= NotNullable || entitySum t)
            (maybe (sqlType p) SqlOther $ listToMaybe $ mapMaybe (T.stripPrefix "sqltype=") $ fieldAttrs fd)
            (def $ fieldAttrs fd)
            (maxLen $ fieldAttrs fd)
            (ref (fieldDB fd) (fieldType fd) (fieldAttrs fd))

    def :: [Attr] -> Maybe Text
    def [] = Nothing
    def (a:as)
        | Just d <- T.stripPrefix "default=" a = Just d
        | otherwise = def as

    maxLen :: [Attr] -> Maybe Integer
    maxLen [] = Nothing
    maxLen (a:as)
        | Just d <- T.stripPrefix "maxlen=" a =
            case reads (T.unpack d) of
              [(i, s)] | all isSpace s -> Just i
              _ -> error $ "Could not parse maxlen field with value " ++
                           show d ++ " on " ++ show tn
        | otherwise = maxLen as

    ref :: DBName
        -> FieldType
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c ft []
        | Just f <- stripId ft =
            Just (resolveTableName allDefs $ HaskellName f, refName tn c)
        | otherwise = Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (a:_)
        | Just x <- T.stripPrefix "reference=" a =
            Just (DBName x, refName tn c)
    ref c x (_:as) = ref c x as

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ mconcat [table, "_", column, "_fkey"]

logSQL :: Q Exp
logSQL = [|\sql_foo params_foo -> $logDebugS (T.pack "SQL") $ T.pack $ show (sql_foo :: Text) ++ " " ++ show (params_foo :: [PersistValue])|]

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | otherwise = NotNullable
