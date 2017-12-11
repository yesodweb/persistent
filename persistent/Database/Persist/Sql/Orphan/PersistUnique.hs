{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Database.Persist.Sql.Orphan.PersistUnique
  ()
  where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist
import Database.Persist.Class.PersistUnique (defaultPutMany, recordEssence)
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Util (dbColumns, parseEntityValues, updatePersistValue, mkUpdateText')
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Reader (ask, withReaderT)
import Data.List (nubBy, reverse)
import Data.Function (on)

defaultUpsert
    :: (MonadIO m
       ,PersistEntity record
       ,PersistUniqueWrite backend
       ,PersistEntityBackend record ~ BaseBackend backend)
    => record -> [Update record] -> ReaderT backend m (Entity record)
defaultUpsert record updates = do
    uniqueKey <- onlyUnique record
    upsertBy uniqueKey record updates

instance PersistUniqueWrite SqlBackend where
    upsert record updates = do
      conn <- ask
      let escape = connEscapeName conn
      let refCol n = T.concat [escape (entityDB t), ".", n]
      let mkUpdateText = mkUpdateText' escape refCol
      uniqueKey <- onlyUnique record
      case connUpsertSql conn of
        Just upsertSql -> case updates of
                            [] -> defaultUpsert record updates
                            _:_ -> do
                                let upds = T.intercalate "," $ map mkUpdateText updates
                                    sql = upsertSql t upds
                                    vals = (map toPersistValue $ toPersistFields record) ++ (map updatePersistValue updates) ++ (unqs uniqueKey)

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

    putMany [] = return ()
    putMany rsD = do
        conn <- ask
        let rs = nubBy ((==) `on` recordEssence) (reverse rsD)
        let ent = entityDef rs
        let nr  = length rs
        let toVals r = (map toPersistValue $ toPersistFields r)
        case connPutManySql conn of
            (Just mkSql) -> rawExecute (mkSql ent nr) (concat (map toVals rs))
            Nothing -> defaultPutMany rs

instance PersistUniqueWrite SqlWriteBackend where
    deleteBy uniq = withReaderT persistBackend $ deleteBy uniq
    upsert rs us = withReaderT persistBackend $ upsert rs us
    putMany rs = withReaderT persistBackend $ putMany rs

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
