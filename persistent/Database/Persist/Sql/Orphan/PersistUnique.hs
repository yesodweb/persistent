module Database.Persist.Sql.Orphan.PersistUnique where

import Database.Persist

instance (C.MonadResource m, MonadLogger m) => PersistUnique (SqlPersist m) where
    deleteBy uniq = do
        conn <- SqlPersist ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        execute' sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = escapeName conn x ++ "=?"
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE "
            , T.intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- SqlPersist ask
        let cols = T.intercalate "," $ (escapeName conn $ entityID t)
                 : map (escapeName conn . fieldDB) (entityFields t)
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE "
                , sqlClause conn
                ]
            vals' = persistUniqueToValues uniq
        R.withStmt sql vals' C.$$ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just (PersistInt64 k:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ unpack s
                        Right x -> return $ Just (Entity (Key $ PersistInt64 k) x)
                Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = escapeName conn x ++ "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames
