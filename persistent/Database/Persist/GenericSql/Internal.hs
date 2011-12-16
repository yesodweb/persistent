{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , rawTableIdName
    , RawName (..)
    , filterClause
    , filterClauseNoWhere
    , getFieldName
    , dummyFromFilts
    , orderClause
    , getFiltsValues
    ) where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Base
import Data.Maybe (fromMaybe)
import Control.Arrow
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl, control, restoreM)
import qualified Control.Exception as E
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
import Control.Exception.Control (bracket)

#define MBCIO MonadControlIO
#endif
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
    , commitC :: (Text -> IO Statement) -> IO ()
    , rollbackC :: (Text -> IO Statement) -> IO ()
    , escapeName :: RawName -> String
    , noLimit :: String
    }
data Statement = Statement
    { finalize :: IO ()
    , reset :: IO ()
    , execute :: [PersistValue] -> IO ()
    , withStmt :: forall a m. (MBCIO m, MonadIO m)
               => [PersistValue] -> (RowPopper m -> m a) -> m a
    }

withSqlPool :: (MonadIO m, MBCIO m)
            => IO Connection -> Int -> (Pool Connection -> m a) -> m a
withSqlPool mkConn = createPool mkConn close'

withSqlConn :: (MonadIO m, MBCIO m) => IO Connection -> (Connection -> m a) -> m a
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
    uniqs = map (RawName *** map (unjustLookup colNameMap))
          $ map (uniqueName &&& uniqueColumns)
          $ entityUniques t
    cols = zipWith go (tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val

    -- Like fromJust . lookup, but gives a more useful error message
    unjustLookup m a = fromMaybe (error $ "Column not found: " ++ a)
                     $ lookup a m

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

getIdNameValue :: [String] -> Maybe String
getIdNameValue (('i':'d':'=':x):_) = Just x
getIdNameValue (_:x) = getIdNameValue x
getIdNameValue [] = Nothing

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

rawTableIdName :: EntityDef -> RawName
rawTableIdName t = RawName $
    case getIdNameValue $ entityAttribs t of
        Nothing -> "id"
        Just x -> x

newtype RawName = RawName { unRawName :: String } -- FIXME Text
    deriving (Eq, Ord)

getFiltsValues :: forall val.  PersistEntity val => Connection -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection -> [Filter val] -> String
filterClause b c = fst . filterClauseHelper b True c

filterClauseNoWhere :: PersistEntity val
                    => Bool -- ^ include table name?
                    -> Connection -> [Filter val] -> String
filterClauseNoWhere b c = fst . filterClauseHelper b False c

filterClauseHelper :: PersistEntity val
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> Connection -> [Filter val] -> (String, [PersistValue])
filterClauseHelper includeTable includeWhere conn filters =
    (if not (null sql) && includeWhere
        then " WHERE " ++ sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (intercalate s $ map wrapP a, concat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = concat ["(", x, ")"]

    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=1", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        case (isNull, pfilter, varCount) of
            (True, Eq, _) -> (name ++ " IS NULL", [])
            (True, Ne, _) -> (name ++ " IS NOT NULL", [])
            (False, Ne, _) -> (concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " <> "
                , qmarks
                , ")"
                ], notNullVals)
            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
            -- not all databases support those words directly.
            (_, In, 0) -> ("1=2", [])
            (False, In, _) -> (name ++ " IN " ++ qmarks, allVals)
            (True, In, _) -> (concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " IN "
                , qmarks
                , ")"
                ], notNullVals)
            (_, NotIn, 0) -> ("1=1", [])
            (False, NotIn, _) -> (concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            (True, NotIn, _) -> (concat
                [ "("
                , name
                , " IS NOT NULL AND "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            _ -> (name ++ showSqlFilter pfilter ++ "?", allVals)
      where
        filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
        filterValueToPersistValues v = map toPersistValue $ either return id v

        isNull = any (== PersistNull) allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        t = entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then (++) (escapeName conn (rawTableName t) ++ ".")
                else id)
            $ escapeName conn $ getFieldName t $ columnName $ persistColumnDef field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in '(' : intercalate "," (map (const "?") x') ++ ")"
        varCount = case value of
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
        showSqlFilter (BackendSpecificFilter s) = s

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

getFieldName :: EntityDef -> String -> RawName
getFieldName t s = rawFieldName $ tableColumn t s

tableColumn :: EntityDef -> String -> ColumnDef
tableColumn t s | s == id_ = ColumnDef id_ "Int64" []
  where id_ = unRawName $ rawTableIdName t
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go (ColumnDef x y z:rest)
        | x == s = ColumnDef x y z
        | otherwise = go rest

dummyFromOrder :: SelectOpt a -> a
dummyFromOrder _ = undefined

orderClause :: PersistEntity val => Bool -> Connection -> SelectOpt val -> String
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x ++ " DESC"
        _ -> error $ "expected Asc or Desc, not limit or offset"
  where
    cd x = persistColumnDef x
    t = entityDef $ dummyFromOrder o
    name x =
        (if includeTable
            then (++) (escapeName conn (rawTableName t) ++ ".")
            else id)
        $ escapeName conn $ getFieldName t $ columnName $ cd x

#if MIN_VERSION_monad_control(0, 3, 0)
bracket :: MonadBaseControl IO m
        => m a       -- ^ computation to run first (\"acquire resource\")
        -> (a -> m b) -- ^ computation to run last (\"release resource\")
        -> (a -> m c) -- ^ computation to run in-between
        -> m c
bracket before after thing = control $ \runInIO ->
                               E.bracket (runInIO before)
                                         (\st -> runInIO $ restoreM st >>= after)
                                         (\st -> runInIO $ restoreM st >>= thing)
#endif
