{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | This is a helper module for creating SQL backends. Regular users do not
-- need to use this module.
module Database.Persist.GenericSql
    ( GenericSql (..)
    , RowPopper
    , insert
    , get
    , replace
    , select
    , count
    , deleteWhere
    , update
    , updateWhere
    , getBy
    , delete
    , deleteBy
    , tableName
    , getFieldName
    , tableColumns
    , toField
    , Column (..)
    , UniqueDef
    , mkColumns
    ) where

import Database.Persist.Base hiding (PersistBackend (..))
import Data.List (intercalate)
import Control.Monad (unless, liftM)
import Data.Int (Int64)
import Control.Arrow (second)
import Control.Arrow
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class

data GenericSql m = GenericSql
    { gsWithStmt :: forall a.
                    String -> [PersistValue] -> (RowPopper m -> m a) -> m a
    , gsExecute :: String -> [PersistValue] -> m ()
    , gsInsert :: String -> [String] -> [PersistValue] -> m Int64
    , gsEntityDefExists :: String -> m Bool
    , gsKeyType :: String
    , gsShowSqlType :: SqlType -> String
    }

type RowPopper m = m (Maybe [PersistValue])

insert :: (Monad m, PersistEntity val)
       => GenericSql m -> val -> m (Key val)
insert gs v = liftM toPersistKey
            . gsInsert gs (tableName t) (map fst3 $ tableColumns t)
            . map toPersistValue . toPersistFields
            $ v
  where
    fst3 (x, _, _) = x
    t = entityDef v

replace :: (PersistEntity v, Monad m)
        => GenericSql m -> Key v -> v -> m ()
replace gs k val = do
    let t = entityDef val
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (go . fst3) $ tableColumns t) ++
              " WHERE id=?"
    gsExecute gs sql $
                    map toPersistValue (toPersistFields val)
                    ++ [PersistInt64 $ fromPersistKey k]
  where
    go = (++ "=?")
    fst3 (x, _, _) = x

dummyFromKey :: Key v -> v
dummyFromKey _ = error "dummyFromKey"

get :: (PersistEntity v, Monad m)
    => GenericSql m -> Key v -> m (Maybe v)
get gs k = do
    let t = entityDef $ dummyFromKey k
    let cols = intercalate "," $ map (\(x, _, _) -> x) $ tableColumns t
    let sql = "SELECT " ++ cols ++ " FROM " ++
              tableName t ++ " WHERE id=?"
    gsWithStmt gs sql [PersistInt64 $ fromPersistKey k] $ \pop -> do
        res <- pop
        case res of
            Nothing -> return Nothing
            Just vals ->
                case fromPersistValues vals of
                    Left e -> error $ "get " ++ showPersistKey k ++ ": " ++ e
                    Right v -> return $ Just v

count :: (PersistEntity val, Monad m)
      => GenericSql m
      -> [Filter val]
      -> m Int
count gs filts = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "SELECT COUNT(*) FROM " ++ tableName t ++ wher
    gsWithStmt gs sql (map persistFilterToValue filts) $ \pop -> do
        Just [PersistInt64 i] <- pop
        return $ fromIntegral i
  where
    t = entityDef $ dummyFromFilts filts

select :: (PersistEntity val, Monad m)
       => GenericSql m
       -> [Filter val]
       -> [Order val]
       -> Int -- ^ limit
       -> Int -- ^ offset
       -> a -- ^ iteration seed
       -> (a -> (Key val, val) -> m (Either a a))
       -> m (Either a a)
select gs filts ords limit offset seed0 iter = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        ord = if null ords
                then ""
                else " ORDER BY " ++
                     intercalate "," (map orderClause ords)
        lim = if limit == 0
                then ""
                else " LIMIT " ++ show lim
        off = if offset == 0
                then ""
                else " OFFSET " ++ show offset
    let cols = intercalate "," $ "id"
               : (map (\(x, _, _) -> x) $ tableColumns t)
    let sql = "SELECT " ++ cols ++ " FROM " ++ tableName t ++ wher ++ ord
              ++ lim ++ off
    gsWithStmt gs sql (map persistFilterToValue filts) $ go seed0
  where
    t = entityDef $ dummyFromFilts filts
    orderClause o = getFieldName t (persistOrderToFieldName o)
                    ++ case persistOrderToOrder o of
                                        Asc -> ""
                                        Desc -> " DESC"
    fromPersistValues' (PersistInt64 x:xs) = do
        case fromPersistValues xs of
            Left e -> Left e
            Right xs' -> Right (toPersistKey x, xs')
    fromPersistValues' _ = Left "error in fromPersistValues'"
    go seed pop = do
        res <- pop
        case res of
            Nothing -> return $ Right seed
            Just vals -> do
                case fromPersistValues' vals of
                    Left s -> error s
                    Right row -> do
                        eseed <- iter seed row
                        case eseed of
                            Left seed' -> return $ Left seed'
                            Right seed' -> go seed' pop

filterClause :: PersistEntity val => Filter val -> String
filterClause f = if persistFilterIsNull f then nullClause else mainClause
  where
    t = entityDef $ dummyFromFilts [f]
    name = getFieldName t $ persistFilterToFieldName f
    mainClause = name ++ showSqlFilter (persistFilterToFilter f) ++ "?"
    nullClause =
        case persistFilterToFilter f of
          Eq -> '(' : mainClause ++ " OR " ++ name ++ " IS NULL)"
          Ne -> '(' : mainClause ++ " OR " ++ name ++ " IS NOT NULL)"
          _ -> mainClause
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="

delete :: (PersistEntity v, Monad m) => GenericSql m -> Key v -> m ()
delete gs k =
    gsExecute gs sql [PersistInt64 $ fromPersistKey k]
  where
    t = entityDef $ dummyFromKey k
    sql = "DELETE FROM " ++ tableName t ++ " WHERE id=?"

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

deleteWhere :: (PersistEntity v, Monad m)
            => GenericSql m -> [Filter v] -> m ()
deleteWhere gs filts = do
    let t = entityDef $ dummyFromFilts filts
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
        sql = "DELETE FROM " ++ tableName t ++ wher
    gsExecute gs sql $ map persistFilterToValue filts

deleteBy :: (PersistEntity v, Monad m) => GenericSql m -> Unique v -> m ()
deleteBy gs uniq =
    gsExecute gs sql $ persistUniqueToValues uniq
  where
    t = entityDef $ dummyFromUnique uniq
    go = map (getFieldName t) . persistUniqueToFieldNames
    sql = "DELETE FROM " ++ tableName t ++ " WHERE " ++
          intercalate " AND " (map (++ "=?") $ go uniq)

update :: (PersistEntity v, Monad m)
       => GenericSql m -> Key v -> [Update v] -> m ()
update _ _ [] = return ()
update gs k upds = do
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map go upds) ++
              " WHERE id=?"
    gsExecute gs sql $
        map persistUpdateToValue upds ++ [PersistInt64 $ fromPersistKey k]
  where
    t = entityDef $ dummyFromKey k
    go = getFieldName t . persistUpdateToFieldName

updateWhere :: (PersistEntity v, MonadIO m)
            => GenericSql m -> [Filter v] -> [Update v] -> m ()
updateWhere _ _ [] = return ()
updateWhere gs filts upds = do
    let wher = if null filts
                then ""
                else " WHERE " ++
                     intercalate " AND " (map filterClause filts)
    let sql = "UPDATE " ++ tableName t ++ " SET " ++
              intercalate "," (map (++ "=?") $ map go upds) ++ wher
    let dat = map persistUpdateToValue upds
           ++ map persistFilterToValue filts
    gsExecute gs sql dat
  where
    t = entityDef $ dummyFromFilts filts
    go = getFieldName t . persistUpdateToFieldName

getBy :: (PersistEntity v, Monad m)
      => GenericSql m -> Unique v -> m (Maybe (Key v, v))
getBy gs uniq = do
    let cols = intercalate "," $ "id"
             : (map (\(x, _, _) -> x) $ tableColumns t)
    let sql = "SELECT " ++ cols ++ " FROM " ++ tableName t ++ " WHERE " ++
              sqlClause
    gsWithStmt gs sql (persistUniqueToValues uniq) $ \pop -> do
        row <- pop
        case row of
            Nothing -> return Nothing
            Just (PersistInt64 k:vals) ->
                case fromPersistValues vals of
                    Left s -> error s
                    Right x -> return $ Just (toPersistKey k, x)
            Just _ -> error "Database.Persist.GenericSql: Bad list in getBy"
  where
    sqlClause = intercalate " AND " $ map (++ "=?") $ toFieldNames' uniq
    t = entityDef $ dummyFromUnique uniq
    toFieldNames' = map (getFieldName t) . persistUniqueToFieldNames

dummyFromUnique :: Unique v -> v
dummyFromUnique _ = error "dummyFromUnique"

tableName :: EntityDef -> String
tableName t =
    case getSqlValue $ entityAttribs t of
        Nothing -> "tbl" ++ entityName t
        Just x -> x

toField :: (String, String, [String]) -> String
toField (n, _, as) =
    case getSqlValue as of
        Just x -> x
        Nothing -> "fld" ++ n

getFieldName :: EntityDef -> String -> String
getFieldName t s = toField $ tableColumn t s

getSqlValue :: [String] -> Maybe String
getSqlValue (('s':'q':'l':'=':x):_) = Just x
getSqlValue (_:x) = getSqlValue x
getSqlValue [] = Nothing

tableColumns :: EntityDef -> [(String, String, [String])]
tableColumns = map (\a@(_, y, z) -> (toField a, y, z)) . entityColumns

tableColumn :: EntityDef -> String -> (String, String, [String])
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go ((x, y, z):rest)
        | x == s = (x, y, z)
        | otherwise = go rest

tableUniques' :: EntityDef -> [(String, [String])]
tableUniques' t = map (second $ map $ getFieldName t) $ entityUniques t

type UniqueDef = (String, [String])

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => val -> ([Column], [UniqueDef])
mkColumns val =
    (cols, uniqs)
  where
    colNameMap = map ((\(x, _, _) -> x) &&& toField) $ entityColumns t
    uniqs = map (second $ map $ fromJust . flip lookup colNameMap) $ entityUniques t
    cols = zipWith go (tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val
    t = entityDef val
    tn = map toLower $ tableName t
    go (name, t', as) p =
        Column name ("null" `elem` as) (sqlType p) (def as) (ref name t' as)
    def [] = Nothing
    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
    def (_:rest) = def rest
    ref c t' [] =
        let l = length t'
            (f, b) = splitAt (l - 2) t'
         in if b == "Id"
                then Just ("tbl" ++ f, refName tn c)
                else Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (('r':'e':'f':'e':'r':'e':'n':'c':'e':'=':x):_) =
        Just (x, refName tn c)
    ref c x (_:y) = ref c x y

refName :: String -> String -> String
refName table column =
    map toLower table ++ '_' : map toLower column ++ "_fkey"

data Column = Column
    { cName :: String
    , _cNull :: Bool
    , _cType :: SqlType
    , _cDefault :: Maybe String
    , _cReference :: (Maybe (String, String)) -- table name, constraint name
    }
