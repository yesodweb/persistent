{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistUnique () where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore ()
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Monad.Logger
import qualified Data.Conduit.List as CL
import Data.Conduit

instance (MonadResource m, MonadLogger m) => PersistUnique (SqlPersistT m) where
    deleteBy uniq = do
        conn <- askSqlConn
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        rawExecute sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = connEscapeName conn x <> "=?"
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , " WHERE "
            , T.intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- askSqlConn
        let cols = T.intercalate "," $ (connEscapeName conn $ entityID t)
                 : map (connEscapeName conn . fieldDB) (entityFields t)
        let sql = T.concat
                [ "SELECT "
                , cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , sqlClause conn
                ]
            vals' = persistUniqueToValues uniq
        rawQuery sql vals' $$ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ T.unpack s
                        Right x -> return $ Just (Entity (Key $ PersistInt64 k) x)
                Just (PersistDouble k:vals) ->   -- oracle
                    case fromPersistValues vals of
                        Left s -> error $ T.unpack s
                        Right x -> return $ Just (Entity (Key $ PersistInt64 $ truncate k) x)
                Just xs -> error $ "Database.Persist.GenericSql: Bad list in getBy xs="++show xs
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = connEscapeName conn x <> "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing
