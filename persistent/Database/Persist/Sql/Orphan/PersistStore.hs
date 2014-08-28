{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Sql.Orphan.PersistStore (withRawQuery, BackendKey(..)) where

import Database.Persist
import Database.Persist.Sql.Types
import Database.Persist.Sql.Raw
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.Text (Text, unpack, pack)
import Data.Monoid (mappend, (<>))
import Control.Monad.IO.Class
import Data.ByteString.Char8 (readInteger)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Acquire (with)
import Data.Int (Int64)
import Web.PathPieces (PathPiece)
import Database.Persist.Sql.Class (PersistFieldSql)
import qualified Data.Aeson as A

withRawQuery :: MonadIO m
             => Text
             -> [PersistValue]
             -> C.Sink [PersistValue] IO a
             -> ReaderT Connection m a
withRawQuery sql vals sink = do
    srcRes <- rawQueryRes sql vals
    liftIO $ with srcRes (C.$$ sink)

instance PersistStore Connection where
    newtype BackendKey SqlBackend = SqlBackendKey { unSqlBackendKey :: Int64 }
        deriving (Show, Read, Eq, Ord, Num, Integral, PersistField, PersistFieldSql, PathPiece, Real, Enum, Bounded, A.ToJSON, A.FromJSON)

    backendKeyToValues (SqlBackendKey i)   = [PersistInt64 i]
    backendKeyFromValues [PersistInt64 i]  = Right $ SqlBackendKey i
    backendKeyFromValues [PersistDouble i] = Right $ SqlBackendKey $ truncate i
    backendKeyFromValues s = Left $ pack $ show s

    update _ [] = return ()
    update k upds = do
        conn <- ask
        let go'' n Assign = n <> "=?"
            go'' n Add = T.concat [n, "=", n, "+?"]
            go'' n Subtract = T.concat [n, "=", n, "-?"]
            go'' n Multiply = T.concat [n, "=", n, "*?"]
            go'' n Divide = T.concat [n, "=", n, "/?"]
        let go' (x, pu) = go'' (connEscapeName conn x) pu
        let wher = case entityPrimary t of
                Just pdef -> T.intercalate " AND " $ map (\fld -> connEscapeName conn (fieldDB fld) <> "=? ") $ primaryFields pdef
                Nothing   -> connEscapeName conn (entityID t) <> "=?"
        let sql = T.concat
                [ "UPDATE "
                , connEscapeName conn $ entityDB t
                , " SET "
                , T.intercalate "," $ map (go' . go) upds
                , " WHERE "
                , wher
                ]
        rawExecute sql $
            map updatePersistValue upds `mappend` keyToValues k
      where
        t = entityDef $ dummyFromKey k
        go x = (fieldDB $ updateFieldDef x, updateUpdate x)

    insert val = do
        conn <- ask
        let esql = connInsertSql conn t vals
        key <-
            case esql of
                ISRSingle sql -> withRawQuery sql vals $ do
                    x <- CL.head
                    case x of
                        Just [PersistInt64 i] -> case keyFromValues [PersistInt64 i] of
                            Left err -> error $ "SQL insert: keyFromValues: PersistInt64 " `mappend` show i `mappend` " " `mappend` unpack err
                            Right k -> return k
                        Nothing -> error $ "SQL insert did not return a result giving the generated ID"
                        Just vals' -> case keyFromValues vals' of
                            Left _ -> error $ "Invalid result from a SQL insert, got: " ++ show vals'
                            Right k -> return k

                ISRInsertGet sql1 sql2 -> do
                    rawExecute sql1 vals
                    withRawQuery sql2 [] $ do
                        mm <- CL.head
                        i <- case mm of
                            Just [PersistInt64 i] -> return $ i
                            Just [PersistDouble i] ->return $ truncate i -- oracle need this!
                            Just [PersistByteString i] -> case readInteger i of -- mssql
                                                            Just (ret,"") -> return $ fromIntegral ret
                                                            xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
                            Just xs -> error $ "invalid sql2 return xs["++show xs++"] sql2["++show sql2++"] sql1["++show sql1++"]"
                            Nothing -> error $ "invalid sql2 returned nothing sql2["++show sql2++"] sql1["++show sql1++"]"
                        case keyFromValues [PersistInt64 i] of
                            Right k -> return k
                            Left err -> error $ "ISRInsertGet: keyFromValues failed: " `mappend` unpack err
                ISRManyKeys sql fs -> do
                    rawExecute sql vals 
                    case entityPrimary t of
                       Nothing -> error $ "ISRManyKeys is used when Primary is defined " ++ show sql
                       Just pdef -> 
                            let pks = map fieldHaskell $ primaryFields pdef
                                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) fs
                            in  case keyFromValues keyvals of
                                    Right k -> return k
                                    Left e  -> error $ "ISRManyKeys: unexpected keyvals result: " `mappend` unpack e

        return key
      where
        t = entityDef $ Just val
        vals = map toPersistValue $ toPersistFields val

    replace k val = do
        conn <- ask
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
            vals = map toPersistValue (toPersistFields val) `mappend` keyToValues k
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
        conn <- ask
        let t = entityDef $ dummyFromKey k
        let cols = T.intercalate ","
                 $ map (connEscapeName conn . fieldDB) $ entityFields t
            noColumns :: Bool
            noColumns = null $ entityFields t
        let wher = case entityPrimary t of
                     Just pdef -> T.intercalate " AND " $ map (\fld -> connEscapeName conn (fieldDB fld) <> "=? ") $ primaryFields pdef
                     Nothing   -> connEscapeName conn (entityID t) <> "=?"
        let sql = T.concat
                [ "SELECT "
                , if noColumns then "*" else cols
                , " FROM "
                , connEscapeName conn $ entityDB t
                , " WHERE "
                , wher
                ]
        withRawQuery sql (keyToValues k) $ do
            res <- CL.head
            case res of
                Nothing -> return Nothing
                Just vals ->
                    case fromPersistValues $ if noColumns then [] else vals of
                        Left e -> error $ "get " ++ show k ++ ": " ++ unpack e
                        Right v -> return $ Just v

    delete k = do
        conn <- ask
        rawExecute (sql conn) (keyToValues k)
      where
        t = entityDef $ dummyFromKey k
        wher conn = 
              case entityPrimary t of
                Just pdef -> T.intercalate " AND " $ map (\fld -> connEscapeName conn (fieldDB fld) <> "=? ") $ primaryFields pdef
                Nothing   -> connEscapeName conn (entityID t) <> "=?"
        sql conn = T.concat
            [ "DELETE FROM "
            , connEscapeName conn $ entityDB t
            , " WHERE "
            , wher conn
            ]

dummyFromKey :: Key v -> Maybe v
dummyFromKey _ = Nothing

insrepHelper :: (MonadIO m, PersistEntity val)
             => Text
             -> Key val
             -> val
             -> ReaderT Connection m ()
insrepHelper command k val = do
    conn <- ask
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
    vals = keyToValues k ++ map toPersistValue (toPersistFields val)

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
updateFieldDef (BackendUpdate {}) = error "updateFieldDef did not expect BackendUpdate"

updatePersistValue :: Update v -> PersistValue
updatePersistValue (Update _ v _) = toPersistValue v
updatePersistValue (BackendUpdate {}) = error "updatePersistValue did not expect BackendUpdate"
