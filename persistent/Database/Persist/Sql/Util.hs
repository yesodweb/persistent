{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Sql.Util (
    parseEntityValues
  , entityColumnNames
  , entityColumnCount
  , isIdField
  , hasCompositeKey
  , dbIdColumns
  , dbIdColumnsEsc
  , dbColumns
) where

import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Database.Persist (
    Entity(Entity), EntityDef, EntityField, HaskellName(HaskellName)
  , PersistEntity, PersistValue
  , keyFromValues, fromPersistValues, fieldDB, entityId, entityPrimary
  , entityFields, fieldHaskell, compositeFields, persistFieldDef
  , DBName)
import Database.Persist.Sql.Types (Sql, SqlBackend, connEscapeName)

entityColumnNames :: EntityDef -> SqlBackend -> [Sql]
entityColumnNames ent conn =
     (if hasCompositeKey ent
      then [] else [connEscapeName conn $ fieldDB (entityId ent)])
  <> map (connEscapeName conn . fieldDB) (entityFields ent)

entityColumnCount :: EntityDef -> Int
entityColumnCount e = length (entityFields e)
                    + if hasCompositeKey e then 0 else 1

hasCompositeKey :: EntityDef -> Bool
hasCompositeKey = isJust . entityPrimary

dbIdColumns :: SqlBackend -> EntityDef -> [Text]
dbIdColumns conn = dbIdColumnsEsc (connEscapeName conn)

dbIdColumnsEsc :: (DBName -> Text) -> EntityDef -> [Text]
dbIdColumnsEsc esc t = case entityPrimary t of
    Just pdef -> map (esc . fieldDB) $ compositeFields pdef
    Nothing   -> [esc $ fieldDB (entityId t)]

dbColumns :: SqlBackend -> EntityDef -> [Text]
dbColumns conn t = case entityPrimary t of
    Just _  -> flds      
    Nothing -> escapeDB (entityId t) : flds
  where
    escapeDB = connEscapeName conn . fieldDB
    flds = map escapeDB (entityFields t)

parseEntityValues :: PersistEntity record
                  => EntityDef -> [PersistValue] -> Either Text (Entity record)
parseEntityValues t vals = 
    case entityPrimary t of
      Just pdef -> 
            let pks = map fieldHaskell $ compositeFields pdef
                keyvals = map snd . filter ((`elem` pks) . fst)
                        $ zip (map fieldHaskell $ entityFields t) vals
            in fromPersistValuesComposite' keyvals vals
      Nothing -> fromPersistValues' vals
  where
    fromPersistValues' (kpv:xs) = -- oracle returns Double 
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' ->
                case keyFromValues [kpv] of
                    Left _ -> error $ "fromPersistValues': keyFromValues failed on " ++ show kpv
                    Right k -> Right (Entity k xs')


    fromPersistValues' xs = Left $ pack ("error in fromPersistValues' xs=" ++ show xs)

    fromPersistValuesComposite' keyvals xs =
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> case keyFromValues keyvals of
                Left _ -> error "fromPersistValuesComposite': keyFromValues failed"
                Right key -> Right (Entity key xs')


isIdField :: PersistEntity record => EntityField record typ -> Bool
isIdField f = fieldHaskell (persistFieldDef f) == HaskellName "Id"
