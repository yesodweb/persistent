{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
-- | Code that is only needed for writing GenericSql backends.
module Database.Persist.GenericSql.Internal
    ( Connection (..)
    , Statement (..)
    , withSqlConn
    , withSqlPool
    , RowPopper
    , mkColumns
    , Column (..)
    , UniqueDef'
    , refName
    , tableColumns
    , rawFieldName
    , rawTableName
    , RawName (..)
    , filterClause
    , getFieldName
    , dummyFromFilts
    , getFiltsValues
    , orderClause
    ) where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Base
import Data.Maybe (fromJust)
import Control.Arrow
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (bracket)
import Database.Persist.Util (nullable)
import Data.List (intercalate)
import Data.Text (Text)

type RowPopper m = m (Maybe [PersistValue])

data Connection = Connection
    { prepare :: Text -> IO Statement
    , insertSql :: RawName -> [RawName] -> Either Text (Text, Text)
    , stmtMap :: IORef (Map.Map Text Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => (Text -> IO Statement) -> v
                 -> IO (Either [Text] [(Bool, Text)])
    , begin :: (Text -> IO Statement) -> IO ()
    , commit :: (Text -> IO Statement) -> IO ()
    , rollback :: (Text -> IO Statement) -> IO ()
    , escapeName :: RawName -> String
    , noLimit :: String
    }
data Statement = Statement
    { finalize :: IO ()
    , reset :: IO ()
    , execute :: [PersistValue] -> IO ()
    , withStmt :: forall a m. MonadControlIO m
               => [PersistValue] -> (RowPopper m -> m a) -> m a
    }

withSqlPool :: MonadControlIO m
            => IO Connection -> Int -> (Pool Connection -> m a) -> m a
withSqlPool mkConn = createPool mkConn close'

withSqlConn :: MonadControlIO m => IO Connection -> (Connection -> m a) -> m a
withSqlConn open = bracket (liftIO open) (liftIO . close')

close' :: Connection -> IO ()
close' conn = do
    readIORef (stmtMap conn) >>= mapM_ finalize . Map.elems
    close conn

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => val -> ([Column], [UniqueDef'])
mkColumns val =
    (cols, uniqs)
  where
    colNameMap = map (columnName &&& rawFieldName) $ entityColumns t
    uniqs = map (RawName *** map (fromJust . flip lookup colNameMap))
          $ map (uniqueName &&& uniqueColumns)
          $ entityUniques t -- FIXME don't use fromJust
    cols = zipWith go (tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val
    t = entityDef val
    tn = rawTableName t
    go (name, t', as) p =
        Column name (nullable as) (sqlType p) (def as) (ref name t' as)
    def [] = Nothing
    def (('d':'e':'f':'a':'u':'l':'t':'=':d):_) = Just d
    def (_:rest) = def rest
    ref c t' [] =
        let l = length t'
            (f, b) = splitAt (l - 2) t'
         in if b == "Id"
                then Just (RawName f, refName tn c)
                else Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (('r':'e':'f':'e':'r':'e':'n':'c':'e':'=':x):_) =
        Just (RawName x, refName tn c)
    ref c x (_:y) = ref c x y

refName :: RawName -> RawName -> RawName
refName (RawName table) (RawName column) =
    RawName $ table ++ '_' : column ++ "_fkey"

data Column = Column
    { cName :: RawName
    , cNull :: Bool
    , cType :: SqlType
    , cDefault :: Maybe String
    , cReference :: (Maybe (RawName, RawName)) -- table name, constraint name
    }

getSqlValue :: [String] -> Maybe String
getSqlValue (('s':'q':'l':'=':x):_) = Just x
getSqlValue (_:x) = getSqlValue x
getSqlValue [] = Nothing

tableColumns :: EntityDef -> [(RawName, String, [String])]
tableColumns = map (\a@(ColumnDef _ y z) -> (rawFieldName a, y, z)) . entityColumns

type UniqueDef' = (RawName, [RawName])

rawFieldName :: ColumnDef -> RawName
rawFieldName (ColumnDef n _ as) = RawName $
    case getSqlValue as of
        Just x -> x
        Nothing -> n

rawTableName :: EntityDef -> RawName
rawTableName t = RawName $
    case getSqlValue $ entityAttribs t of
        Nothing -> entityName t
        Just x -> x

newtype RawName = RawName { unRawName :: String } -- FIXME Text
    deriving (Eq, Ord)

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection -> Filter val -> String
filterClause includeTable conn f =
    case (isNull, filterFilter f, varCount) of
        (True, Eq, _) -> name ++ " IS NULL"
        (True, Ne, _) -> name ++ " IS NOT NULL"
        (False, Ne, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , "<>?)"
            ]
        -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
        -- not all databases support those words directly.
        (_, In, 0) -> "1=2"
        (False, In, _) -> name ++ " IN " ++ qmarks
        (True, In, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " IN "
            , qmarks
            , ")"
            ]
        (_, NotIn, 0) -> "1=1"
        (False, NotIn, _) -> concat
            [ "("
            , name
            , " IS NULL OR "
            , name
            , " NOT IN "
            , qmarks
            , ")"
            ]
        (True, NotIn, _) -> concat
            [ "("
            , name
            , " IS NOT NULL AND "
            , name
            , " NOT IN "
            , qmarks
            , ")"
            ]
        _ -> name ++ showSqlFilter (filterFilter f) ++ "?"
  where
    isNull = any (== PersistNull)
           $ either return id
           $ filterPersistValue f
    t = entityDef $ dummyFromFilts [f]
    name =
        (if includeTable
            then (++) (escapeName conn (rawTableName t) ++ ".")
            else id)
        $ escapeName conn $ getFieldName t $ filterName f
    qmarks = case filterPersistValue f of
                Left _ -> "?"
                Right x ->
                    let x' = filter (/= PersistNull) x
                     in '(' : intercalate "," (map (const "?") x') ++ ")"
    varCount = case filterPersistValue f of
                Left _ -> 1
                Right x -> length x
    showSqlFilter Eq = "="
    showSqlFilter Ne = "<>"
    showSqlFilter Gt = ">"
    showSqlFilter Lt = "<"
    showSqlFilter Ge = ">="
    showSqlFilter Le = "<="
    showSqlFilter In = " IN "
    showSqlFilter NotIn = " NOT IN "

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

getFieldName :: EntityDef -> String -> RawName
getFieldName t s = rawFieldName $ tableColumn t s

tableColumn :: EntityDef -> String -> ColumnDef
tableColumn _ "id" = ColumnDef "id" "Int64" []
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go (ColumnDef x y z:rest)
        | x == s = ColumnDef x y z
        | otherwise = go rest

getFiltsValues :: PersistEntity val => [Filter val] -> [PersistValue]
getFiltsValues =
    concatMap $ go . filterPersistValue
  where
    go (Left PersistNull) = []
    go (Left x) = [x]
    go (Right xs) = filter (/= PersistNull) xs

dummyFromOrder :: Order a -> a
dummyFromOrder _ = undefined

orderClause :: PersistEntity val => Bool -> Connection -> Order val -> String
orderClause includeTable conn o =
    name ++ case o of
                Asc _ -> ""
                Desc _ -> " DESC"
  where
    cd = case o of
                Asc x -> persistColumnDef x
                Desc x -> persistColumnDef x
    t = entityDef $ dummyFromOrder o
    name =
        (if includeTable
            then (++) (escapeName conn (rawTableName t) ++ ".")
            else id)
        $ escapeName conn $ getFieldName t $ columnName cd

filterPersistValue :: Filter v -> Either PersistValue [PersistValue]
filterPersistValue (Filter _ v _) = either (Left . toPersistValue) (Right . map toPersistValue) v

filterName :: PersistEntity v => Filter v -> String
filterName (Filter f _ _) = columnName $ persistColumnDef f
