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
    , tableColumn
    , rawFieldName
    , rawTableName
    , rawTableIdName
    , RawName (..)
    , getFieldName
    ) where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Store
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

tableColumn :: EntityDef -> String -> ColumnDef
tableColumn t s | s == id_ = ColumnDef id_ "Int64" []
  where id_ = unRawName $ rawTableIdName t
tableColumn t s = go $ entityColumns t
  where
    go [] = error $ "Unknown table column: " ++ s
    go (ColumnDef x y z:rest)
        | x == s = ColumnDef x y z
        | otherwise = go rest

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

getFieldName :: EntityDef -> String -> RawName
getFieldName t s = rawFieldName $ tableColumn t s

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
