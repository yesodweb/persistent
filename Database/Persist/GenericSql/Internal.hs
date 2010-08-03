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
    , tableName
    , Column (..)
    , UniqueDef
    , refName
    , tableColumns
    , toField
    ) where

import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.IO.Class
import Database.Persist.Pool
import Data.Char (toLower)
import Database.Persist.Base
import Data.Maybe (fromJust)
import Control.Arrow

type RowPopper m = m (Maybe [PersistValue])

data Connection = Connection
    { prepare :: String -> IO Statement
    , insertSql :: String -> [String] -> Either String (String, String)
    , stmtMap :: IORef (Map.Map String Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => (String -> IO Statement) -> v
                 -> IO (Either [String] [(Bool, String)])
    , begin :: (String -> IO Statement) -> IO ()
    , commit :: (String -> IO Statement) -> IO ()
    , rollback :: (String -> IO Statement) -> IO ()
    , likeOperator :: String
    }
data Statement = Statement
    { finalize :: IO ()
    , reset :: IO ()
    , execute :: [PersistValue] -> IO ()
    , withStmt :: forall a m. MonadCatchIO m
               => [PersistValue] -> (RowPopper m -> m a) -> m a
    }

withSqlPool :: MonadCatchIO m
            => IO Connection -> Int -> (Pool Connection -> m a) -> m a
withSqlPool mkConn = createPool mkConn close'

withSqlConn :: MonadCatchIO m => IO Connection -> (Connection -> m a) -> m a
withSqlConn open = bracket (liftIO open) (liftIO . close')

close' :: Connection -> IO ()
close' conn = do
    readIORef (stmtMap conn) >>= mapM_ finalize . Map.elems
    close conn

-- | Create the list of columns for the given entity.
mkColumns :: PersistEntity val => val -> ([Column], [UniqueDef])
mkColumns val =
    (cols, uniqs)
  where
    colNameMap = map ((\(x, _, _) -> x) &&& toField) $ entityColumns t
    uniqs = map (second $ map $ fromJust . flip lookup colNameMap) $ entityUniques t -- FIXME don't use fromJust
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

getSqlValue :: [String] -> Maybe String
getSqlValue (('s':'q':'l':'=':x):_) = Just x
getSqlValue (_:x) = getSqlValue x
getSqlValue [] = Nothing

tableColumns :: EntityDef -> [(String, String, [String])]
tableColumns = map (\a@(_, y, z) -> (toField a, y, z)) . entityColumns

type UniqueDef = (String, [String])
