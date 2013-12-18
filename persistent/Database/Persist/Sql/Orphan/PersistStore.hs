{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
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
import Data.Monoid (mappend, (<>))
import Control.Monad.IO.Class
import Data.ByteString.Char8 (readInteger)
import Data.Maybe (isJust)
import Data.List (find)

instance (C.MonadResource m, MonadLogger m) => PersistStore (SqlPersistT m) where
    type MonadBackend (SqlPersistT m) = SqlBackend
    insert val = do
        conn <- askSqlConn
        case connInsertSql conn t vals of
            ISRSingle sql -> do
                x <- rawQuery sql vals C.$$ CL.head
                return $
                    case x of
                        Just [p@(PersistInt64 _)] -> persistValueToPersistKey p
                        Nothing -> error "SQL insert did not return a result giving the generated ID"
                        Just vals' -> error $ "Invalid result from a SQL insert, got: " ++ show vals'

            ISRInsertGet sql1 sql2 -> do
                rawExecute sql1 vals
                mm <- rawQuery sql2 [] C.$$ CL.head
                return $
                    case mm of
                      Just [p@(PersistInt64 _)] -> persistValueToPersistKey p
                      Just [PersistDouble i]    -> persistValueToPersistKey $ PersistInt64 $ truncate i -- oracle need this!
                      Just [PersistByteString i] -> case readInteger i of -- mssql
                                                      Just (ret,"") -> persistValueToPersistKey $ PersistInt64 $ fromIntegral ret
                                                      xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
                      Just xs -> error $ "invalid sql2 return xs["++show xs++"] sql2["++show sql2++"] sql1["++show sql1++"]"
                      Nothing -> error $ "invalid sql2 returned nothing sql2["++show sql2++"] sql1["++show sql1++"]"

            ISRManyKeys sql fs -> do
                rawExecute sql vals 
                return $
                    case entityPrimary t of
                       Nothing -> error $ "ISRManyKeys is used when Primary is defined " ++ show sql
                       Just pdef -> 
                            let pks = map fst $ primaryFields pdef
                                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) fs
                            in persistValueToPersistKey (PersistList keyvals)

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
            vals = map toPersistValue (toPersistFields val) `mappend` [persistKeyToPersistValue k]
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
        let wher = case entityPrimary t of
                     Just pdef -> T.intercalate " AND " $ map (\fld -> connEscapeName conn (snd fld) <> "=? ") $ primaryFields pdef
                     Nothing   -> connEscapeName conn (entityID t) <> "=?"
        let sql = T.concat
                [ "SELECT "
                , cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , wher
                ]
        rawQuery sql [persistKeyToPersistValue k] C.$$ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues vals of
                        Left e -> error $ "get " ++ show k ++ ": " ++ unpack e
                        Right v -> return $ Just v

    delete k = do
        conn <- askSqlConn
        rawExecute (sql conn) [persistKeyToPersistValue k]
      where
        t = entityDef $ dummyFromKey k
        wher conn = 
              case entityPrimary t of
                Just pdef -> T.intercalate " AND " $ map (\fld -> connEscapeName conn (snd fld) <> "=? ") $ primaryFields pdef
                Nothing   -> connEscapeName conn (entityID t) <> "=?"
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , " WHERE "
            , wher conn
            ]

dummyFromKey :: Key v -> Maybe v
dummyFromKey _ = Nothing

insrepHelper :: ( MonadIO m,  MonadLogger m, MonadSqlPersist m
               , PersistEntity record, EntityBackend record ~ SqlBackend
               )
             => Text
             -> Key record
             -> record
             -> m ()
insrepHelper command k val = do
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
    vals = persistKeyToPersistValue k : map toPersistValue (toPersistFields val)
