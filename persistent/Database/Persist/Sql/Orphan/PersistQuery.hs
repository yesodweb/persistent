{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistQuery where

import Database.Persist

-- orphaned instance for convenience of modularity
instance (MonadResource m, MonadLogger m) => PersistQuery (SqlPersist m) where
    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Assign = n ++ "=?"
            go'' n Add = concat [n, "=", n, "+?"]
            go'' n Subtract = concat [n, "=", n, "-?"]
            go'' n Multiply = concat [n, "=", n, "*?"]
            go'' n Divide = concat [n, "=", n, "/?"]
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = concat
                [ "UPDATE "
                , escapeName conn $ entityDB t
                , " SET "
                , T.intercalate "," $ map (go' . go) upds
                , " WHERE "
                , escapeName conn $ entityID t
                , "=?"
                ]
        execute' sql $
            map updatePersistValue upds `mappend` [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ entityDB t
                , wher
                ]
        R.withStmt sql (getFiltsValues conn filts) $$ do
            Just [PersistInt64 i] <- CL.head
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectSource filts opts = do
        conn <- lift $ SqlPersist ask
        R.withStmt (sql conn) (getFiltsValues conn filts) $= CL.mapM parse
      where
        (limit, offset, orders) = limitOffsetOrder opts

        parse vals =
            case fromPersistValues' vals of
                Left s -> liftIO $ throwIO $ PersistMarshalError s
                Right row -> return row

        t = entityDef $ dummyFromFilts filts
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (Entity (Key $ PersistInt64 x) xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ T.intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' $ noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = T.intercalate ","
                  $ (escapeName conn $ entityID t)
                  : map (escapeName conn . fieldDB) (entityFields t)
        sql conn = concat
            [ "SELECT "
            , cols conn
            , " FROM "
            , escapeName conn $ entityDB t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

    selectKeys filts opts = do
        conn <- lift $ SqlPersist ask
        R.withStmt (sql conn) (getFiltsValues conn filts) $= CL.mapM parse
      where
        parse [PersistInt64 i] = return $ Key $ PersistInt64 i
        parse y = liftIO $ throwIO $ PersistMarshalError $ "Unexpected in selectKeys: " ++ show y
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = concat
            [ "SELECT "
            , escapeName conn $ entityID t
            , " FROM "
            , escapeName conn $ entityDB t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

        (limit, offset, orders) = limitOffsetOrder opts

        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ T.intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> T.cons ' ' $ noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset

    deleteWhere filts = do
        _ <- deleteWhereCount filts
        return ()

    updateWhere filts upds = do
        _ <- updateWhereCount filts upds
        return ()
