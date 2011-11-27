{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.GenericSql.Query 
  ( PersistQuery (..),
    SqlPersist (..)
  )
  where

import Database.Persist.Store
import Database.Persist.Query
import Database.Persist.GenericSql
import Database.Persist.GenericSql.ConvertFilters
import Database.Persist.GenericSql.Internal
import qualified Database.Persist.GenericSql.Raw as R

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.List (intercalate)
import Data.Text (Text, pack)

import Data.Enumerator hiding (consume, map)

import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception (toException)


instance MonadControlIO m => PersistQuery SqlPersist m where
    update _ [] = return ()
    update k upds = do
        conn <- SqlPersist ask
        let go'' n Assign = n ++ "=?"
            go'' n Add = n ++ '=' : n ++ "+?"
            go'' n Subtract = n ++ '=' : n ++ "-?"
            go'' n Multiply = n ++ '=' : n ++ "*?"
            go'' n Divide = n ++ '=' : n ++ "/?"
        let go' (x, pu) = go'' (escapeName conn x) pu
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' . go) upds
                , " WHERE id=?"
                ]
        execute' sql $
            map updatePersistValue upds ++ [unKey k]
      where
        t = entityDef $ dummyFromKey k
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

    count filts = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "SELECT COUNT(*) FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        withStmt' sql (getFiltsValues conn filts) $ \pop -> do
            Just [PersistInt64 i] <- pop
            return $ fromIntegral i
      where
        t = entityDef $ dummyFromFilts filts

    selectEnum filts opts =
        Iteratee . start
      where
        (limit, offset, orders) = limitOffsetOrder opts

        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just vals -> do
                    case fromPersistValues' vals of
                        Left s -> return $ Error $ toException
                                $ PersistMarshalError s
                        Right row -> do
                            step <- runIteratee $ k $ Chunks [row]
                            loop step pop
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        fromPersistValues' (PersistInt64 x:xs) = do
            case fromPersistValues xs of
                Left e -> Left e
                Right xs' -> Right (Key $ PersistInt64 x, xs')
        fromPersistValues' _ = Left "error in fromPersistValues'"
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        ord conn =
            case map (orderClause False conn) orders of
                [] -> ""
                ords -> " ORDER BY " ++ intercalate "," ords
        lim conn = case (limit, offset) of
                (0, 0) -> ""
                (0, _) -> ' ' : noLimit conn
                (_, _) -> " LIMIT " ++ show limit
        off = if offset == 0
                    then ""
                    else " OFFSET " ++ show offset
        cols conn = intercalate "," $ (unRawName $ rawTableIdName t)
                   : (map (\(x, _, _) -> escapeName conn x) $ tableColumns t)
        sql conn = pack $ concat
            [ "SELECT "
            , cols conn
            , " FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            , ord conn
            , lim conn
            , off
            ]

    selectKeys filts =
        Iteratee . start
      where
        start x = do
            conn <- SqlPersist ask
            withStmt' (sql conn) (getFiltsValues conn filts) $ loop x
        loop (Continue k) pop = do
            res <- pop
            case res of
                Nothing -> return $ Continue k
                Just [PersistInt64 i] -> do
                    step <- runIteratee $ k $ Chunks [Key $ PersistInt64 i]
                    loop step pop
                Just y -> return $ Error $ toException $ PersistMarshalError
                        $ "Unexpected in selectKeys: " ++ show y
        loop step _ = return step
        t = entityDef $ dummyFromFilts filts
        wher conn = if null filts
                    then ""
                    else filterClause False conn filts
        sql conn = pack $ concat
            [ "SELECT id FROM "
            , escapeName conn $ rawTableName t
            , wher conn
            ]

    deleteWhere filts = do
        conn <- SqlPersist ask
        let t = entityDef $ dummyFromFilts filts
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
            sql = pack $ concat
                [ "DELETE FROM "
                , escapeName conn $ rawTableName t
                , wher
                ]
        execute' sql $ getFiltsValues conn filts

    updateWhere _ [] = return ()
    updateWhere filts upds = do
        conn <- SqlPersist ask
        let wher = if null filts
                    then ""
                    else filterClause False conn filts
        let sql = pack $ concat
                [ "UPDATE "
                , escapeName conn $ rawTableName t
                , " SET "
                , intercalate "," $ map (go' conn . go) upds
                , wher
                ]
        let dat = map updatePersistValue upds ++ getFiltsValues conn filts
        execute' sql dat
      where
        t = entityDef $ dummyFromFilts filts
        go'' n Assign = n ++ "=?"
        go'' n Add = n ++ '=' : n ++ "+?"
        go'' n Subtract = n ++ '=' : n ++ "-?"
        go'' n Multiply = n ++ '=' : n ++ "*?"
        go'' n Divide = n ++ '=' : n ++ "/?"
        go' conn (x, pu) = go'' (escapeName conn x) pu
        go x = ( getFieldName t $ updateFieldName x
               , updateUpdate x
               )

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v

dummyFromKey :: Key SqlPersist v -> v 
dummyFromKey _ = error "dummyFromKey"

execute' :: MonadIO m => Text -> [PersistValue] -> SqlPersist m ()
execute' = R.execute

withStmt' :: MonadControlIO m => Text -> [PersistValue]
         -> (RowPopper (SqlPersist m) -> SqlPersist m a) -> SqlPersist m a
withStmt' = R.withStmt
