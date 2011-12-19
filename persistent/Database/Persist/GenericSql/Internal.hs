{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
-- | Code that is only needed for writing GenericSql backends.
module Database.Persist.GenericSql.Internal
    ( Connection (..)
    , Statement (..)
    , withSqlConn
    , withSqlPool
    , RowPopper
    , mkColumns
    , Column (..)
    , filterClause
    , filterClauseNoWhere
    , dummyFromFilts
    , orderClause
    , getFiltsValues
    ) where

import Prelude hiding ((++))
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
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Data.Monoid (Monoid, mappend, mconcat)
import Database.Persist.EntityDef

type RowPopper m = m (Maybe [PersistValue])

data Connection = Connection
    { prepare :: Text -> IO Statement
    -- ^ table name, column names, either 1 or 2 statements to run
    , insertSql :: DBName -> [DBName] -> Either Text (Text, Text)
    , stmtMap :: IORef (Map.Map Text Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => [EntityDef]
                 -> (Text -> IO Statement)
                 -> v
                 -> IO (Either [Text] [(Bool, Text)])
    , begin :: (Text -> IO Statement) -> IO ()
    , commitC :: (Text -> IO Statement) -> IO ()
    , rollbackC :: (Text -> IO Statement) -> IO ()
    , escapeName :: DBName -> Text
    , noLimit :: Text
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

resolveTableName :: [EntityDef] -> HaskellName -> DBName
resolveTableName [] (HaskellName hn) = error $ "Table not found: " `mappend` T.unpack hn
resolveTableName (e:es) hn
    | entityHaskell e == hn = entityDB e
    | otherwise = resolveTableName es hn

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => [EntityDef] -> val -> ([Column], [UniqueDef])
mkColumns allDefs val = error "mkColumns"
    (cols, entityUniques t)
  where
    cols :: [Column]
    cols = zipWith go (entityFields t)
         $ toPersistFields
         $ halfDefined `asTypeOf` val

    t :: EntityDef
    t = entityDef val

    tn :: DBName
    tn = entityDB t

    go :: FieldDef -> SomePersistField -> Column
    go fd p =
        Column
            (fieldDB fd)
            (nullable $ fieldAttrs fd)
            (sqlType p)
            (def $ fieldAttrs fd)
            (ref (fieldDB fd) (fieldType fd) (fieldAttrs fd))

    def :: [Attr] -> Maybe Text
    def [] = Nothing
    def (a:as)
        | Just d <- T.stripPrefix "default=" a = Just d
        | otherwise = def as

    ref :: DBName
        -> FieldType
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c (FieldType t') []
        | Just f <- T.stripSuffix "Id" t' =
            Just (resolveTableName allDefs $ HaskellName f, refName tn c)
        | otherwise = Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (a:_)
        | Just x <- T.stripPrefix "reference=" a =
            Just (DBName x, refName tn c)
    ref c x (_:as) = ref c x as

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ mconcat [table, "_", column, "_fkey"]

data Column = Column
    { cName      :: DBName
    , cNull      :: Bool
    , cType      :: SqlType
    , cDefault   :: Maybe Text
    , cReference :: (Maybe (DBName, DBName)) -- table name, constraint name
    }

{- FIXME
getSqlValue :: [String] -> Maybe String
getSqlValue (('s':'q':'l':'=':x):_) = Just x
getSqlValue (_:x) = getSqlValue x
getSqlValue [] = Nothing

getIdNameValue :: [String] -> Maybe String
getIdNameValue (('i':'d':'=':x):_) = Just x
getIdNameValue (_:x) = getIdNameValue x
getIdNameValue [] = Nothing
-}

{- FIXME
tableColumns :: EntityDef -> [(RawName, String, [String])]
tableColumns = map (\a@(ColumnDef _ y z) -> (rawFieldName a, y, z)) . entityColumns
-}

getFiltsValues :: forall val.  PersistEntity val => Connection -> [Filter val] -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn

filterClause :: PersistEntity val
             => Bool -- ^ include table name?
             -> Connection
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c

filterClauseNoWhere :: PersistEntity val
                    => Bool -- ^ include table name?
                    -> Connection
                    -> [Filter val]
                    -> Text
filterClauseNoWhere b c = fst . filterClauseHelper b False c

filterClauseHelper :: PersistEntity val
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> Connection
             -> [Filter val]
             -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn filters =
    (if not (T.null sql) && includeWhere
        then " WHERE " ++ sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (intercalate s $ map wrapP a, concat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=1", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        case (isNull, pfilter, varCount) of
            (True, Eq, _) -> (name ++ " IS NULL", [])
            (True, Ne, _) -> (name ++ " IS NOT NULL", [])
            (False, Ne, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " ++ "
                , qmarks
                , ")"
                ], notNullVals)
            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
            -- not all databases support those words directly.
            (_, In, 0) -> ("1=2", [])
            (False, In, _) -> (name ++ " IN " ++ qmarks, allVals)
            (True, In, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " IN "
                , qmarks
                , ")"
                ], notNullVals)
            (_, NotIn, 0) -> ("1=1", [])
            (False, NotIn, _) -> (T.concat
                [ "("
                , name
                , " IS NULL OR "
                , name
                , " NOT IN "
                , qmarks
                , ")"
                ], notNullVals)
            (True, NotIn, _) -> (T.concat
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
        tn = escapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn ++ ".") ++)
                else id)
            $ escapeName conn $ fieldDB $ persistFieldDef field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" ++ intercalate "," (map (const "?") x') ++ ")"
        varCount = case value of
                    Left _ -> 1
                    Right x -> length x
        showSqlFilter Eq = "="
        showSqlFilter Ne = "++"
        showSqlFilter Gt = ">"
        showSqlFilter Lt = "<"
        showSqlFilter Ge = ">="
        showSqlFilter Le = "<="
        showSqlFilter In = " IN "
        showSqlFilter NotIn = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s

dummyFromFilts :: [Filter v] -> v
dummyFromFilts _ = error "dummyFromFilts"

{- FIXME
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
-}

dummyFromOrder :: SelectOpt a -> a
dummyFromOrder _ = undefined

orderClause :: PersistEntity val
            => Bool -- ^ include the table name
            -> Connection
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x ++ " DESC"
        _ -> error $ "orderClause: expected Asc or Desc, not limit or offset"
  where
    tn = escapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name x =
        (if includeTable
            then ((tn ++ ".") ++)
            else id)
        $ escapeName conn $ fieldDB $ persistFieldDef x

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

infixr 5 ++
(++) :: Text -> Text -> Text
(++) = mappend
