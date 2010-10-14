{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
-- | Code that is only needed for writing GenericSql backends.
module Database.Persist.GenericSql.Internal
    ( Connection (..)
    , Statement (..)
    , withSqlConn
    , withSqlPool
    , withSqlPoolF
    , RowPopper
    , mkColumns
    , Column (..)
    , UniqueDef
    , refName
    , tableColumns
    , rawFieldName
    , rawTableName
    , RawName (..)
    ) where

import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.IO.Class
import Database.Persist.Pool
import Database.Persist.Base
import Data.Maybe (fromJust)
import Control.Arrow

type RowPopper m = m (Maybe [PersistValue])

data Connection = Connection
    { prepare :: String -> IO Statement
    , insertSql :: RawName -> [RawName] -> Either String (String, String)
    , stmtMap :: IORef (Map.Map String Statement)
    , close :: IO ()
    , migrateSql :: forall v. PersistEntity v
                 => (String -> IO Statement) -> v
                 -> IO (Either [String] [(Bool, String)])
    , begin :: (String -> IO Statement) -> IO ()
    , commit :: (String -> IO Statement) -> IO ()
    , rollback :: (String -> IO Statement) -> IO ()
    , escapeName :: RawName -> String
    , noLimit :: String
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

withSqlPoolF :: MonadIO m
             => (m a -> m () -> m a)
             -> IO Connection -> Int -> (Pool Connection -> m a) -> m a
withSqlPoolF finally' mkConn = createPoolF finally' mkConn close'

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
    colNameMap = map ((\(x, _, _) -> x) &&& rawFieldName) $ entityColumns t
    uniqs = map (RawName *** map (fromJust . flip lookup colNameMap))
          $ entityUniques t -- FIXME don't use fromJust
    cols = zipWith go (tableColumns t) $ toPersistFields $ halfDefined `asTypeOf` val
    t = entityDef val
    tn = rawTableName t
    go (name, t', as) p =
        Column name ("null" `elem` as) (sqlType p) (def as) (ref name t' as)
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
tableColumns = map (\a@(_, y, z) -> (rawFieldName a, y, z)) . entityColumns

type UniqueDef = (RawName, [RawName])

rawFieldName :: (String, String, [String]) -> RawName
rawFieldName (n, _, as) = RawName $
    case getSqlValue as of
        Just x -> x
        Nothing -> n

rawTableName :: EntityDef -> RawName
rawTableName t = RawName $
    case getSqlValue $ entityAttribs t of
        Nothing -> entityName t
        Just x -> x

newtype RawName = RawName { unRawName :: String }
    deriving (Eq, Ord)
