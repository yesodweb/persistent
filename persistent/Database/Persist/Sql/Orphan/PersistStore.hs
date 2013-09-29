{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistStore () where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Class
import Database.Persist.Sql.Raw
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Data.Monoid (mappend)
import Control.Monad.IO.Class
import Data.ByteString.Char8 (readInt,readInteger)

instance (C.MonadResource m, MonadLogger m) => PersistStore (SqlPersistT m) where
    type PersistMonadBackend (SqlPersistT m) = SqlBackend
    insert val = do
        conn <- askSqlConn
        let esql = connInsertSql conn (entityDB t) (entityFields t) (entityID t) vals
        i <-
            case esql of
                ISRSingle sql -> rawQuery sql vals C.$$ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> return i
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> error $ "Invalid result from a SQL insert, got: " ++ show vals'
                ISRInsertGet sql1 sql2 -> do
                    rawExecute sql1 vals
                    rawQuery sql2 [] C.$$ do
                        mm <- CL.head
                        case mm of
                          Just [PersistInt64 i] -> return i
                          Just [PersistDouble i] -> return $ truncate i -- oracle
                          Just [PersistByteString i] -> case readInteger i of -- mssql
                                                          Just (ret,"") -> return $ fromIntegral ret
                                                          xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
                          Just xs -> error $ "invalid sql2 return xs["++show xs++"] sql2["++show sql2++"] sql1["++show sql1++"]"
                          Nothing -> error $ "invalid sql2 returned nothing sql2["++show sql2++"] sql1["++show sql1++"]"
        return $ Key $ PersistInt64 i
      where
        t = entityDef $ Just val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- askSqlConn
        let t = entityDef $ Just val
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn (entityDB t)
                , " SET "
                , T.intercalate "," (map (go conn . fieldDB) $ entityFields t)
                , " WHERE "
                , connEscapeName conn $ entityID t
                , "=?"
                ]
            vals = map toPersistValue (toPersistFields val) `mappend` [unKey k]
        rawExecute sql vals
      where
        go conn x = connEscapeName conn x `T.append` "=?"

    insertKey = insrepHelper "INSERT"

    repsert key value = do
        mExisting <- get key
        case mExisting of
          Nothing -> insertKey key value
          Just _ -> replace key value

    get k = do
        conn <- askSqlConn
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (connEscapeName conn . fieldDB) $ entityFields t
        let sql = T.concat
                [ "SELECT "
                , cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , connEscapeName conn $ entityID t
                , "=?"
                ]
            vals' = [unKey k]
        rawQuery sql vals' C.$$ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ "get " ++ show (unKey k) ++ ": " ++ unpack e
                        Right v -> return $ Just v

    delete k = do
        conn <- askSqlConn
        rawExecute (sql conn) [unKey k]
      where
        t = entityDef $ dummyFromKey k
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , " WHERE "
            , connEscapeName conn $ entityID t
            , "=?"
            ]

dummyFromKey :: KeyBackend SqlBackend v -> Maybe v
dummyFromKey _ = Nothing

insrepHelper :: (MonadIO m, PersistEntity val, MonadLogger m, MonadSqlPersist m)
             => Text
             -> Key val
             -> val
             -> m ()
insrepHelper command (Key k) val = do
    conn <- askSqlConn
    rawExecute (sql conn) vals
  where
    t = entityDef $ Just val
    sql conn = T.concat
        [ command
        , " INTO "
        , connEscapeName conn (entityDB t)
        , "("
        , T.intercalate ","
            $ map (connEscapeName conn)
            $ entityID t : map fieldDB (entityFields t)
        , ") VALUES("
        , T.intercalate "," ("?" : map (const "?") (entityFields t))
        , ")"
        ]
    vals = k : map toPersistValue (toPersistFields val)
