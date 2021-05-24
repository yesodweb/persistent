{-# LANGUAGE ExplicitForAll  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Database.Persist.Sql.Orphan.PersistUnique
  ()
  where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Conduit.List as CL
import Data.Function (on)
import Data.List (nubBy)
import qualified Data.Text as T
import Data.Foldable (toList)

import Database.Persist
import Database.Persist.Class.PersistUnique (defaultUpsertBy, defaultPutMany, persistUniqueKeyValues)

import Database.Persist.Sql.Types.Internal
import Database.Persist.Sql.Raw
import Database.Persist.Sql.Orphan.PersistStore (withRawQuery)
import Database.Persist.Sql.Util (dbColumns, parseEntityValues, updatePersistValue, mkUpdateText')

instance PersistUniqueWrite SqlBackend where
    upsertBy uniqueKey record updates = do
      conn <- ask
      let refCol n = T.concat [connEscapeTableName conn t, ".", n]
      let mkUpdateText = mkUpdateText' (connEscapeFieldName conn) refCol
      case connUpsertSql conn of
        Just upsertSql -> case updates of
                            [] -> defaultUpsertBy uniqueKey record updates
                            _:_ -> do
                                let upds = T.intercalate "," $ map mkUpdateText updates
                                    sql = upsertSql t (persistUniqueToFieldNames uniqueKey) upds
                                    vals = map toPersistValue (toPersistFields record)
                                        ++ map updatePersistValue updates
                                        ++ unqs uniqueKey

                                x <- rawSql sql vals
                                return $ head x
        Nothing -> defaultUpsertBy uniqueKey record updates
        where
          t = entityDef $ Just record
          unqs uniqueKey' = concatMap persistUniqueToValues [uniqueKey']

    deleteBy uniq = do
        conn <- ask
        let sql' = sql conn
            vals = persistUniqueToValues uniq
        rawExecute sql' vals
      where
        t = entityDef $ dummyFromUnique uniq
        go = toList . fmap snd . persistUniqueToFieldNames
        go' conn x = connEscapeFieldName conn x `mappend` "=?"
        sql conn =
            T.concat
                [ "DELETE FROM "
                , connEscapeTableName conn t
                , " WHERE "
                , T.intercalate " AND " $ map (go' conn) $ go uniq]

    putMany [] = return ()
    putMany rsD = do
        let uKeys = persistUniqueKeys . head $ rsD
        case uKeys of
            [] -> insertMany_ rsD
            _ -> go
        where
          go = do
            let rs = nubBy ((==) `on` persistUniqueKeyValues) (reverse rsD)
            let ent = entityDef rs
            let nr  = length rs
            let toVals r = map toPersistValue $ toPersistFields r
            conn <- ask
            case connPutManySql conn of
                (Just mkSql) -> rawExecute (mkSql ent nr) (concatMap toVals rs)
                Nothing -> defaultPutMany rs

instance PersistUniqueWrite SqlWriteBackend where
    deleteBy uniq = withBaseBackend $ deleteBy uniq
    upsert rs us = withBaseBackend $ upsert rs us
    putMany rs = withBaseBackend $ putMany rs

instance PersistUniqueRead SqlBackend where
    getBy uniq = do
        conn <- ask
        let sql =
                T.concat
                    [ "SELECT "
                    , T.intercalate "," $ toList $ dbColumns conn t
                    , " FROM "
                    , connEscapeTableName conn t
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
        go conn x = connEscapeFieldName conn x `mappend` "=?"
        t = entityDef $ dummyFromUnique uniq
        toFieldNames' = toList . fmap snd . persistUniqueToFieldNames

instance PersistUniqueRead SqlReadBackend where
    getBy uniq = withBaseBackend $ getBy uniq

instance PersistUniqueRead SqlWriteBackend where
    getBy uniq = withBaseBackend $ getBy uniq

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing
