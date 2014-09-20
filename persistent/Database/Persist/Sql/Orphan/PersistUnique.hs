{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistUnique () where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import qualified Data.Text as T
import Data.Monoid (mappend)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Reader (ask)

instance PersistUnique Connection where
    deleteBy uniq = do
        conn <- ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        rawExecute sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = map snd . persistUniqueToFieldNames
        go' conn x = connEscapeName conn x `mappend` "=?"
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , " WHERE "
            , T.intercalate " AND " $ map (go' conn) $ go uniq
            ]

    getBy uniq = do
        conn <- ask
        let flds = map (connEscapeName conn . fieldDB) (entityFields t)
        let cols = case entityPrimary t of
                     Just _ -> T.intercalate "," flds
                     Nothing -> T.intercalate "," $ connEscapeName conn (fieldDB (entityId t)) : flds
        let sql = T.concat
                [ "SELECT "
                , cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , sqlClause conn
                ]
            vals' = persistUniqueToValues uniq
        withRawQuery sql vals' $ do
            row <- CL.head
            case row of
                Nothing -> return Nothing
                Just [] -> error "getBy: empty row"
                Just (kpv:vals) ->
                    case fromPersistValues vals of
                        Left s -> error $ T.unpack s
                        Right x ->
                            case keyFromValues [kpv] of
                                Right k -> return $ Just (Entity k x)
                                Left _ -> error $ "getBy: keyFromValues failed: " `mappend` show kpv
      where
        sqlClause conn =
            T.intercalate " AND " $ map (go conn) $ toFieldNames' uniq
        go conn x = connEscapeName conn x `mappend` "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = map snd . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing
