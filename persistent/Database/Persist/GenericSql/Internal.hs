{-# LANGUAGE TemplateHaskell #-}
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
    , createSqlPool
    , mkColumns
    , Column (..)
    , logSQL
    ) where

import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.IORef
import Control.Monad.IO.Class
import Data.Conduit.Pool
import Database.Persist.Store
import Control.Exception.Lifted (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Persist.Util (nullable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid (Monoid, mappend, mconcat)
import Database.Persist.EntityDef
import qualified Data.Conduit as C
import Language.Haskell.TH.Syntax (Q, Exp)
import Control.Monad.Logger (logOther)
import Data.Maybe (mapMaybe, listToMaybe)

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
    , withStmt :: forall m. C.MonadResource m
               => [PersistValue]
               -> C.Source m [PersistValue]
    }

withSqlPool :: MonadIO m
            => IO Connection -- ^ create a new connection
            -> Int -- ^ connection count
            -> (Pool Connection -> m a)
            -> m a
withSqlPool mkConn connCount f = do
    pool <- createSqlPool mkConn connCount
    f pool

createSqlPool :: MonadIO m
              => IO Connection
              -> Int
              -> m (Pool Connection)
createSqlPool mkConn = liftIO . createPool mkConn close' 1 20

withSqlConn :: (MonadIO m, MonadBaseControl IO m)
            => IO Connection -> (Connection -> m a) -> m a
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
mkColumns allDefs val =
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
            (nullable (fieldAttrs fd) || entitySum t)
            (maybe (sqlType p) SqlOther $ listToMaybe $ mapMaybe (T.stripPrefix "sqltype=") $ fieldAttrs fd)
            (def $ fieldAttrs fd)
            (maxLen $ fieldAttrs fd)
            (ref (fieldDB fd) (fieldType fd) (fieldAttrs fd))

    def :: [Attr] -> Maybe Text
    def [] = Nothing
    def (a:as)
        | Just d <- T.stripPrefix "default=" a = Just d
        | otherwise = def as

    maxLen :: [Attr] -> Maybe Integer
    maxLen [] = Nothing
    maxLen (a:as)
        | Just d <- T.stripPrefix "maxlen=" a =
            case reads (T.unpack d) of
              [(i, s)] | all isSpace s -> Just i
              _ -> error $ "Could not parse maxlen field with value " ++
                           show d ++ " on " ++ show tn
        | otherwise = maxLen as

    ref :: DBName
        -> FieldType
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c ft []
        | Just f <- stripId ft =
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
    , cMaxLen    :: Maybe Integer
    , cReference :: (Maybe (DBName, DBName)) -- table name, constraint name
    }
    deriving (Eq, Ord, Show)

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

logSQL :: Q Exp
logSQL = [|\sql_foo params_foo -> $(logOther "SQL") $ T.pack $ show (sql_foo :: Text) ++ " " ++ show (params_foo :: [PersistValue])|]
