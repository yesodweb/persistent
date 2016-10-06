{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Database.Persist.Sql.Orphan.PersistUnique
  ()
  where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Util (dbColumns, parseEntityValues)
import qualified Data.Text as T
import Data.Monoid (mappend, (<>))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Reader (ask, withReaderT)
import Control.Monad (when, liftM)

defaultUpsert
    :: (MonadIO m
       ,PersistEntity record
       ,PersistUniqueWrite backend
       ,PersistEntityBackend record ~ BaseBackend backend)
    => record -> [Update record] -> ReaderT backend m (Entity record)
defaultUpsert record updates = do
    uniqueKey <- onlyUnique record
    upsertBy uniqueKey record updates

escape :: DBName -> T.Text
escape (DBName s) = T.pack $ '"' : go (T.unpack s) ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

instance PersistUniqueWrite SqlBackend where
    upsert record updates = do
        conn <- ask
        uniqueKey <- onlyUnique record
        case connUpsertSql conn of
            Just upsertSql ->
                case updates of
                    [] -> defaultUpsert record updates
                    xs -> do
                        let upds = T.intercalate "," $ map (go' . go) updates
                            sql = upsertSql t upds
                            vals =
                                (map toPersistValue $ toPersistFields record) ++
                                (map updatePersistValue updates) ++
                                (unqs uniqueKey)
                            go'' n Assign =
                                escape (entityDB t) <> "." <> n <> "=?"
                            go'' n Add =
                                T.concat
                                    [ n
                                    , "="
                                    , escape (entityDB t) <> "."
                                    , n
                                    , "+?"]
                            go'' n Subtract =
                                T.concat
                                    [ n
                                    , "="
                                    , escape (entityDB t) <> "."
                                    , n
                                    , "-?"]
                            go'' n Multiply =
                                T.concat
                                    [ n
                                    , "="
                                    , escape (entityDB t) <> "."
                                    , n
                                    , "*?"]
                            go'' n Divide = T.concat [n, "=", n, "/?"]
                            go'' _ (BackendSpecificUpdate up) =
                                error $
                                T.unpack $
                                "BackendSpecificUpdate" `mappend` up `mappend`
                                "not supported"
                            go' (x,pu) = go'' (connEscapeName conn x) pu
                            go x = (fieldDB $ updateFieldDef x, updateUpdate x)
                        x <- rawSql sql vals
                        return $ head x
            Nothing -> defaultUpsert record updates
      where
        t = entityDef $ Just record
        unqs uniqueKey = concat $ map (persistUniqueToValues) [uniqueKey]
    deleteBy uniq = do
        conn <- ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        rawExecute sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = connEscapeName conn x `mappend` "=?"
        sql conn =
            T.concat
                [ "DELETE FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , T.intercalate " AND " $ map (go' conn) $ go uniq]

instance PersistUniqueWrite SqlWriteBackend where
    deleteBy uniq = withReaderT persistBackend $ deleteBy uniq

instance PersistUniqueRead SqlBackend where
    getBy uniq = do
        conn <- ask
        let sql =
                T.concat
                    [ "SELECT "
                    , T.intercalate "," $ dbColumns conn t
                    , " FROM "
                    , connEscapeName conn $ entityDB t
                    , " WHERE "
                    , sqlClause conn]
            uvals = persistUniqueToValues uniq
        withRawQuery sql uvals $
            do row <- CL.head
               case row of
                   Nothing -> return Nothing
                   Just [] -> error "getBy: empty row"
                   Just vals ->
                       case parseEntityValues t vals of
                           Left err ->
                               liftIO $ throwIO $ PersistMarshalError err
                           Right r -> return $ Just r
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = connEscapeName conn x `mappend` "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

instance PersistUniqueRead SqlReadBackend where
    getBy uniq = withReaderT persistBackend $ getBy uniq

instance PersistUniqueRead SqlWriteBackend where
    getBy uniq = withReaderT persistBackend $ getBy uniq

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing

updateFieldDef
    :: PersistEntity v
    => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
updateFieldDef (BackendUpdate{}) =
    error "updateFieldDef did not expect BackendUpdate"

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate{}) =
    error "updatePersistValue did not expect BackendUpdate"