module Database.Persist.Sql.Util (
    parseEntityValues
  , entityColumnNames
  , entityColumnCount
) where

import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.List (find)
import Data.Text (Text, pack)
import Database.Persist (
    Entity(Entity), EntityDef
  , PersistEntity, PersistValue, PersistException(PersistMarshalError)
  , keyFromValues, fromPersistValues, fieldDB, entityId, entityPrimary
  , entityFields, fieldHaskell, compositeFields)
import Database.Persist.Sql.Types (Sql, SqlBackend, connEscapeName)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception (throwIO)

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

parseEntityValues :: PersistEntity record
                  => EntityDef -> [PersistValue] -> Either Text (Entity record)
parseEntityValues t vals = 
    case entityPrimary t of
      Just pdef -> 
            let pks = map fieldHaskell $ compositeFields pdef
                keyvals = map snd $ filter (\(a, _) -> let ret=isJust (find (== a) pks) in ret) $ zip (map fieldHaskell $ entityFields t) vals
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


