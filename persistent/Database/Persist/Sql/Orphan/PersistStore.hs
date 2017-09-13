{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistStore
  ( withRawQuery
  , BackendKey(..)
  , toSqlKey
  , fromSqlKey
  , getFieldName
  , getTableName
  , tableDBName
  , fieldDBName
  ) where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Util (dbIdColumns, keyAndEntityColumnNames)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Data.Monoid (mappend, (<>))
import Control.Monad.IO.Class
import Data.ByteString.Char8 (readInteger)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad.Trans.Reader (ReaderT, ask, withReaderT)
import Data.Acquire (with)
import Data.Int (Int64)
import Web.PathPieces (PathPiece)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)
import Database.Persist.Sql.Class (PersistFieldSql)
import qualified Data.Aeson as A
import Control.Exception.Lifted (throwIO)

withRawQuery :: MonadIO m
             => Text
             -> [PersistValue]
             -> C.Sink [PersistValue] IO a
             -> ReaderT (SqlBackend db) m a
withRawQuery sql vals sink = do
    srcRes <- rawQueryRes sql vals
    liftIO $ with srcRes (C.$$ sink)

toSqlKey :: ToBackendKey (SqlBackend db) record => Int64 -> Key record
toSqlKey = fromBackendKey . SqlBackendKey

fromSqlKey :: ToBackendKey (SqlBackend db) record => Key record -> Int64
fromSqlKey = unSqlBackendKey . toBackendKey

whereStmtForKey 
    :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend db) 
    => SqlBackend db -> Key record -> Text
whereStmtForKey conn k =
    T.intercalate " AND "
  $ map (<> "=? ")
  $ dbIdColumns conn entDef
  where
    entDef = entityDef $ dummyFromKey k


-- | get the SQL string for the table that a PeristEntity represents
-- Useful for raw SQL queries
--
-- Your backend may provide a more convenient tableName function
-- which does not operate in a Monad
getTableName :: forall record m backend db.
             ( PersistEntity record
             , PersistEntityBackend record ~ SqlBackend db
             , IsSqlBackend backend db
             , Monad m
             ) => record -> ReaderT backend m Text
getTableName rec = withReaderT persistBackend $ do
    conn <- ask
    return $ connEscapeName conn $ tableDBName rec

-- | useful for a backend to implement tableName by adding escaping
tableDBName :: (PersistEntity record) => record -> DBName
tableDBName rec = entityDB $ entityDef (Just rec)

-- | get the SQL string for the field that an EntityField represents
-- Useful for raw SQL queries
--
-- Your backend may provide a more convenient fieldName function
-- which does not operate in a Monad
getFieldName :: forall record typ m backend db.
             ( PersistEntity record
             , PersistEntityBackend record ~ SqlBackend db
             , IsSqlBackend backend db
             , Monad m
             )
             => EntityField record typ -> ReaderT backend m Text
getFieldName rec = withReaderT persistBackend $ do
    conn <- ask
    return $ connEscapeName conn $ fieldDBName rec

-- | useful for a backend to implement fieldName by adding escaping
fieldDBName :: forall record typ. (PersistEntity record) => EntityField record typ -> DBName
fieldDBName = fieldDB . persistFieldDef


instance PersistCore (SqlBackend db) where
    newtype BackendKey (SqlBackend db) = SqlBackendKey { unSqlBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)
instance PersistCore (SqlReadBackend db) where
    newtype BackendKey (SqlReadBackend db) = SqlReadBackendKey { unSqlReadBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)
instance PersistCore (SqlWriteBackend db) where
    newtype BackendKey (SqlWriteBackend db) = SqlWriteBackendKey { unSqlWriteBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

instance PersistStoreWrite (SqlBackend db) where
    update _ [] = return ()
    update k upds = do
        conn <- ask
        let go'' n Assign = n <> "=?"
            go'' n Add = T.concat [n, "=", n, "+?"]
            go'' n Subtract = T.concat [n, "=", n, "-?"]
            go'' n Multiply = T.concat [n, "=", n, "*?"]
            go'' n Divide = T.concat [n, "=", n, "/?"]
            go'' _ (BackendSpecificUpdate up) = error $ T.unpack $ "BackendSpecificUpdate" `Data.Monoid.mappend` up `mappend` "not supported"
        let go' (x, pu) = go'' (connEscapeName conn x) pu
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn $ tableDBName $ recordTypeFromKey k
                , " SET "
                , T.intercalate "," $ map (go' . go) upds
                , " WHERE "
                , wher
                ]
        rawExecute sql $
            map updatePersistValue upds `mappend` keyToValues k
      where
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

    insert val = do
        conn <- ask
        let esql = connInsertSql conn t vals
        key <-
            case esql of
                ISRSingle sql -> withRawQuery sql vals $ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> case keyFromValues [PersistInt64 i] of
                            Left err -> error $ "SQL insert: keyFromValues: PersistInt64 " `mappend` show i `mappend` " " `mappend` unpack err
                            Right k -> return k
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> case keyFromValues vals' of
                            Left _ -> error $ "Invalid result from a SQL insert, got: " ++ show vals'
                            Right k -> return k

                ISRInsertGet sql1 sql2 -> do
                    rawExecute sql1 vals
                    withRawQuery sql2 [] $ do
                        mm <- CL.head
                        let m = maybe
                                  (Left $ "No results from ISRInsertGet: " `mappend` tshow (sql1, sql2))
                                  Right mm

                        -- TODO: figure out something better for MySQL
                        let convert x =
                                case x of
                                    [PersistByteString i] -> case readInteger i of -- mssql
                                                            Just (ret,"") -> [PersistInt64 $ fromIntegral ret]
                                                            _ -> x
                                    _ -> x
                            -- Yes, it's just <|>. Older bases don't have the
                            -- instance for Either.
                            onLeft Left{} x = x
                            onLeft x _ = x

                        case m >>= (\x -> keyFromValues x `onLeft` keyFromValues (convert x)) of
                            Right k -> return k
                            Left err -> throw $ "ISRInsertGet: keyFromValues failed: " `mappend` err
                ISRManyKeys sql fs -> do
                    rawExecute sql vals
                    case entityPrimary t of
                       Nothing -> error $ "ISRManyKeys is used when Primary is defined " ++ show sql
                       Just pdef ->
                            let pks = map fieldHaskell $ compositeFields pdef
                                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) fs
                            in  case keyFromValues keyvals of
                                    Right k -> return k
                                    Left e  -> error $ "ISRManyKeys: unexpected keyvals result: " `mappend` unpack e

        return key
      where
        tshow :: Show a => a -> Text
        tshow = T.pack . show
        throw = liftIO . throwIO . userError . T.unpack
        t = entityDef $ Just val
        vals = map toPersistValue $ toPersistFields val

    insertMany [] = return []
    insertMany vals = do
        conn <- ask

        case connInsertManySql conn of
            Nothing -> mapM insert vals
            Just insertManyFn -> do
                case insertManyFn ent valss of
                    ISRSingle sql -> rawSql sql (concat valss)
                    _ -> error "ISRSingle is expected from the connInsertManySql function"
                where
                    ent = entityDef vals
                    valss = map (map toPersistValue . toPersistFields) vals



    insertMany_ [] = return ()
    insertMany_ vals0 = do conn <- ask
                           case connMaxParams conn of
                            Nothing -> insertMany_' vals0
                            Just maxParams -> let chunkSize = maxParams `div` length (entityFields t) in
                                                mapM_ insertMany_' (chunksOf chunkSize vals0)
      where
        insertMany_' vals = do
          conn <- ask
          let valss = map (map toPersistValue . toPersistFields) vals
          let sql = T.concat
                  [ "INSERT INTO "
                  , connEscapeName conn (entityDB t)
                  , "("
                  , T.intercalate "," $ map (connEscapeName conn . fieldDB) $ entityFields t
                  , ") VALUES ("
                  , T.intercalate "),(" $ replicate (length valss) $ T.intercalate "," $ map (const "?") (entityFields t)
                  , ")"
                  ]
          rawExecute sql (concat valss)

        t = entityDef vals0
        -- Implement this here to avoid depending on the split package
        chunksOf _ [] = []
        chunksOf size xs = let (chunk, rest) = splitAt size xs in chunk : chunksOf size rest

    replace k val = do
        conn <- ask
        let t = entityDef $ Just val
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE "
                , wher
                ]
            vals = map toPersistValue (toPersistFields val) `mappend` keyToValues k
        rawExecute sql vals
      where
        go conn x = connEscapeName conn x `T.append` "=?"

    insertKey = insrepHelper "INSERT"

    repsert key value = do
        mExisting <- get key
        case mExisting of
          Nothing -> insertKey key value
          Just _ -> replace key value

    delete k = do
        conn <- ask
        rawExecute (sql conn) (keyToValues k)
      where
        wher conn = whereStmtForKey conn k
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ tableDBName $ recordTypeFromKey k
            , " WHERE "
            , wher conn
            ]
instance PersistStoreWrite (SqlWriteBackend db) where
    insert v = withReaderT persistBackend $ insert v
    insertMany vs = withReaderT persistBackend $ insertMany vs
    insertMany_ vs = withReaderT persistBackend $ insertMany_ vs
    insertKey k v = withReaderT persistBackend $ insertKey k v
    repsert k v = withReaderT persistBackend $ repsert k v
    replace k v = withReaderT persistBackend $ replace k v
    delete k = withReaderT persistBackend $ delete k
    update k upds = withReaderT persistBackend $ update k upds


instance PersistStoreRead (SqlBackend db) where
    get k = do
        conn <- ask
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (connEscapeName conn . fieldDB) $ entityFields t
            noColumns :: Bool
            noColumns = null $ entityFields t
        let wher = whereStmtForKey conn k
        let sql = T.concat
                [ "SELECT "
                , if noColumns then "*" else cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , wher
                ]
        withRawQuery sql (keyToValues k) $ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues $ if noColumns then [] else vals of
                        Left e -> error $ "get " ++ show k ++ ": " ++ unpack e
                        Right v -> return $ Just v
instance PersistStoreRead (SqlReadBackend db) where
    get k = withReaderT persistBackend $ get k
instance PersistStoreRead (SqlWriteBackend db) where
    get k = withReaderT persistBackend $ get k

dummyFromKey :: Key record -> Maybe record
dummyFromKey = Just . recordTypeFromKey

recordTypeFromKey :: Key record -> record
recordTypeFromKey _ = error "dummyFromKey"

insrepHelper :: (MonadIO m, PersistEntity val)
             => Text
             -> Key val
             -> val
             -> ReaderT (SqlBackend db) m ()
insrepHelper command k record = do
    conn <- ask
    let columnNames = keyAndEntityColumnNames entDef conn
    rawExecute (sql conn columnNames) vals
  where
    entDef = entityDef $ Just record
    sql conn columnNames = T.concat
        [ command
        , " INTO "
        , connEscapeName conn (entityDB entDef)
        , "("
        , T.intercalate "," columnNames
        , ") VALUES("
        , T.intercalate "," (map (const "?") columnNames)
        , ")"
        ]
    vals = entityValues (Entity k record)

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
updateFieldDef (BackendUpdate {}) = error "updateFieldDef did not expect BackendUpdate"

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate {}) = error "updatePersistValue did not expect BackendUpdate"
