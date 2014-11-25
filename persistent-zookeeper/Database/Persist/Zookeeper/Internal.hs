{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Zookeeper.Internal
       where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Monoid
import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.Text as T
import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Zookeeper.Binary
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Map as M



txtToKey :: (PersistEntity val) => String -> Key val
txtToKey txt =
  case (keyFromValues [PersistText (T.pack txt)]) of
    Right v -> v
    Left _v ->
      case B64.decode $ B.pack txt of
        Left v -> error $ v
        Right v' ->
          case A.decode $ BL.fromStrict v' of
            Just values ->
              case (keyFromValues values) of
                Right v -> v
                Left v -> error $ T.unpack v
            Nothing -> error "failed"

keyToTxt :: (PersistEntity val) => Key val -> String
keyToTxt key =
  case keyToValues key of
    [PersistText txt] -> T.unpack txt
    v -> B.unpack $ B64.encode $ BL.toStrict $ A.encode $ v

dummyFromKey :: Key v -> Maybe v
dummyFromKey _ = Nothing

dummyFromFList :: [Filter v] -> v
dummyFromFList _ = error "huga"

dummyFromUnique :: Unique v -> Maybe v
dummyFromUnique _ = Nothing

val2table :: (PersistEntity val) => val -> T.Text
val2table = unDBName . entityDB . entityDef . Just

uniqkey2key :: (PersistEntity val) => Unique val -> Key val
uniqkey2key uniqkey = txtToKey $ (B.unpack $ B64.encode $ BL.toStrict $ A.encode $ persistUniqueToValues uniqkey)

val2uniqkey :: (MonadIO m, PersistEntity val) => val -> m (Maybe (Unique val))
val2uniqkey val = do
  case persistUniqueKeys val of
    (uniqkey:_) -> return $ Just uniqkey
    [] -> return Nothing

entity2bin :: (PersistEntity val) => val -> B.ByteString
entity2bin val = toValue (map toPersistValue (toPersistFields val))

bin2entity :: (PersistEntity val) => B.ByteString -> Maybe val
bin2entity bin =
  case fromPersistValues (fromValue bin) of
    Right body  -> Just $ body
    Left s -> error $ T.unpack s

entity2path :: (PersistEntity val) => val -> String
entity2path val = "/" <> (T.unpack $ val2table val)

key2path :: (PersistEntity val) => Key val -> String
key2path key = entity2path $ fromJust $ dummyFromKey key

filter2path :: (PersistEntity val) => [Filter val] -> String
filter2path filterList = entity2path $ dummyFromFList filterList

getMap :: PersistEntity val => val -> M.Map T.Text PersistValue
getMap val =  M.fromList $ getList val

getList :: PersistEntity val => val -> [(T.Text,PersistValue)]
getList val =
  let fields = fmap toPersistValue (toPersistFields val)
      in zip (getFieldsName val) fields

getFieldsName :: (PersistEntity val) => val -> [T.Text]
getFieldsName val =  fmap (unDBName.fieldDB) $ entityFields $ entityDef $ Just val

getFieldName :: (PersistEntity val) => EntityField val typ -> T.Text
getFieldName field =  unDBName $ fieldDB $ persistFieldDef $ field

fieldval :: (PersistEntity val) => EntityField val typ -> val -> PersistValue
fieldval field val = (getMap val) M.! (getFieldName field)

updateEntity :: PersistEntity val =>  val -> [Update val] -> Either T.Text val
updateEntity val upds =
  fromPersistValues $ map snd $ foldl updateVals (getList val) upds

updateVals :: PersistEntity val =>  [(T.Text,PersistValue)] -> Update val -> [(T.Text,PersistValue)]
updateVals [] _ = []
updateVals ((k,v):xs) u@(Update field _ _) =
  if getFieldName field == k
    then (k,updateVal v u):xs
    else (k,v):updateVals xs u
updateVals a _ = error $"not supported vals:" ++ show a

updateVal :: PersistEntity val =>  PersistValue -> Update val -> PersistValue
updateVal _org (BackendUpdate _) = error $ "BackendUpdate is not supported."
updateVal org (Update _ val upd) =
  case upd of
    Assign -> pval
    Add -> numAdd org pval
    Subtract -> numSub org pval
    Multiply -> numMul org pval
    Divide -> numDiv org pval
    BackendSpecificUpdate _ -> error $ "BackendSpecificUpdate is not supported."
  where
    pval = toPersistValue val
    numAdd (PersistInt64 l) (PersistInt64 r) =  (PersistInt64 (l + r))
    numAdd (PersistNull) (PersistInt64 r) =  (PersistInt64 r)
    numAdd (PersistDouble l) (PersistDouble r) =  (PersistDouble (l + r))
    numAdd (PersistNull) (PersistDouble r) =  (PersistDouble r)
    numAdd o _  = error $ "not support : " ++ show o
    numSub (PersistInt64 l) (PersistInt64 r) =  (PersistInt64 (l - r))
    numSub (PersistNull) (PersistInt64 r) =  (PersistInt64 (0 - r))
    numSub (PersistDouble l) (PersistDouble r) =  (PersistDouble (l - r))
    numSub (PersistNull) (PersistDouble r) =  (PersistDouble (0 - r))
    numSub _ _  = error "not support"
    numMul (PersistInt64 l) (PersistInt64 r) =  (PersistInt64 (l * r))
    numMul (PersistNull) (PersistInt64 r) =  (PersistInt64 (0 * r))
    numMul (PersistDouble l) (PersistDouble r) =  (PersistDouble (l * r))
    numMul (PersistNull) (PersistDouble r) =  (PersistDouble (0 * r))
    numMul _ _  = error "not support"
    numDiv (PersistInt64 l) (PersistInt64 r) =  (PersistInt64 (l `div` r))
    numDiv (PersistNull) (PersistInt64 r) =  (PersistInt64 (0 `div` r))
    numDiv (PersistDouble l) (PersistDouble r) =  (PersistDouble (l / r))
    numDiv (PersistNull) (PersistDouble r) =  (PersistDouble (0 / r))
    numDiv _ _  = error "not support"
