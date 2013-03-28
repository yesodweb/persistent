module Database.Persist.Sql.Orphan.PersistStore where

import Database.Persist

instance (C.MonadResource m, MonadLogger m) => PersistStore (SqlPersist m) where
    type PersistMonadBackend (SqlPersist m) = R.SqlBackend
    insert val = do
        conn <- SqlPersist ask
        let esql = insertSql conn (entityDB t) (map fieldDB $ entityFields t) (entityID t)
        i <-
            case esql of
                ISRSingle sql -> R.withStmt sql vals C.$$ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> return i
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> error $ "Invalid result from a SQL insert, got: " P.++ P.show vals'
                ISRInsertGet sql1 sql2 -> do
                    execute' sql1 vals
                    R.withStmt sql2 [] C.$$ do
                        Just [PersistInt64 i] <- CL.head
                        return i
        return $ Key $ PersistInt64 i
      where
        t = entityDef val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- SqlPersist ask
        let t = entityDef val
        let sql = concat
                [ "UPDATE "
                , escapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE "
                , escapeName conn $ entityID t
                , "=?"
                ]
            vals = map toPersistValue (toPersistFields val) `mappend` [unKey k]
        execute' sql vals
      where
        go conn x = escapeName conn x ++ "=?"

    insertKey = insrepHelper "INSERT"

    repsert key value = do
        -- FIXME use this for sqlite insrepHelper "REPLACE"
        delete key
        insertKey key value

    get k = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (escapeName conn . fieldDB) $ entityFields t
        let sql = concat
                [ "SELECT "
                , cols
                , " FROM "
                , escapeName conn $ entityDB t
                , " WHERE "
                , escapeName conn $ entityID t
                , "=?"
                ]
            vals' = [unKey k]
        R.withStmt sql vals' C.$$ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ unpack $ "get " ++ show (unKey k) ++ ": " ++ e
                        Right v -> return $ Just v

    delete k = do
        conn <- SqlPersist ask
        execute' (sql conn) [unKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = concat
            [ "DELETE FROM "
            , escapeName conn $ entityDB t
            , " WHERE "
            , escapeName conn $ entityID t
            , "=?"
            ]
