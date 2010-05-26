{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map as Map
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (first)
import qualified Database.SQLite3 as D
import Data.Int (Int64)
import Data.List (intercalate)

class Monad m => HasTable val m where
    data Key val m
    -- something about unique?

    insert :: val -> m (Key val m)
    replace :: Key val m -> val -> m ()

    get :: Key val m -> m (Maybe (val))

    delete :: Key val m -> m ()

class HasTable val m => HasUpdateTable val m where
    update :: Key val m -> [Field val] -> m ()

class HasTable val m => HasSelectTable val m where
    select :: [Filter val] -> m ([(Key val m, val)])

data Person = Person String Int
    deriving Show

class HasField a where
    data Field a
    updateField :: Field a -> a -> a

instance HasField Person where
    data Field Person = PersonName String | PersonAge Int
    updateField (PersonName name) (Person _ age) = Person name age
    updateField (PersonAge age) (Person name _) = Person name age

class HasFilter a where
    data Filter a
    applyFilter :: Filter a -> a -> Bool

instance HasFilter Person where
    data Filter Person = PersonNameEq String | PersonAgeLt Int
    applyFilter (PersonNameEq x) (Person y _) = x == y
    applyFilter (PersonAgeLt x) (Person _ y) = y < x

instance Monad m => HasTable v (S.StateT (Map.Map Int v) m) where
    data Key v (S.StateT (Map.Map Int v) m) = MapKey { unMapKey :: !Int }
        deriving Show

    insert p = do
        m <- S.get
        let pid = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
        S.put $ Map.insert pid p m
        return $ MapKey pid
    replace (MapKey pid) = S.modify . Map.insert pid

    get pid = Map.lookup (unMapKey pid) `liftM` S.get

    delete = S.modify . Map.delete . unMapKey

instance (Monad m, HasField v)
      => HasUpdateTable v (S.StateT (Map.Map Int v) m) where
    update (MapKey k) us = S.modify $ \m ->
        let moldVal = Map.lookup k m
         in case moldVal of
                Nothing -> m
                Just oldVal ->
                    let newVal = foldr updateField oldVal us
                     in Map.insert k newVal m

instance (Monad m, HasFilter v)
      => HasSelectTable v (S.StateT (Map.Map Int v) m) where
    select fs = (map (first MapKey) . filter go . Map.toList) `liftM` S.get
      where
        go (_, val) = all (flip applyFilter val) fs

initDb :: SqlTable v => D.Database -> v -> IO ()
initDb db x = do
    s <- D.prepare db $ createTable x
    D.Done <- D.step s
    D.finalize s

class SqlTable v where
    tableName :: v -> String
    toSQLData :: v -> [D.SQLData]
    fromSQLData :: [D.SQLData] -> Either String v
    columnNames :: v -> [String]
    createTable :: v -> String
    createTable v =
        "CREATE TABLE " ++ tableName v ++ " (id INTEGER PRIMARY KEY" ++
        concatMap (',' :) (columnNames v) ++ ")"

class SqlTable v => SqlUpdateTable v where
    toSQLName :: Field v -> String
    updateToSQLData :: Field v -> D.SQLData

instance SqlTable v => HasTable v (R.ReaderT D.Database IO) where
    data Key v (R.ReaderT D.Database IO) = DbKey { unDbKey :: !Int64 }
        deriving Show

    insert v = R.ask >>= \conn -> liftIO $ do
        let sql = "INSERT INTO " ++ tableName v ++ " VALUES(NULL" ++
                  concatMap (\_ -> ",?") (columnNames v) ++ ")"
        s <- D.prepare conn sql
        D.bind s $ toSQLData v
        D.Done <- D.step s
        D.finalize s
        s2 <- D.prepare conn "SELECT last_insert_rowid()"
        D.Row <- D.step s2
        [D.SQLInteger i] <- D.columns s2
        D.Done <- D.step s2
        D.finalize s2
        return $ DbKey i

    replace (DbKey k) v = R.ask >>= \conn -> liftIO $ do
        let sql = "REPLACE INTO " ++ tableName v ++ " VALUES(?" ++
                  concatMap (\_ -> ",?") (columnNames v) ++ ")"
        s <- D.prepare conn sql
        D.bind s $ D.SQLInteger k : toSQLData v
        D.Done <- D.step s
        D.finalize s

    get k = R.ask >>= \conn -> liftIO $ do
        let sql = "SELECT * FROM " ++ tableName (undefined :: v) ++ " WHERE id=?"
        s <- D.prepare conn sql
        D.bind s [D.SQLInteger $ unDbKey k]
        row <- D.step s
        res <- case row of
                    D.Done -> return Nothing
                    D.Row -> do
                        _:cols <- D.columns s
                        either error (return . Just) $ fromSQLData cols
        D.finalize s
        return res

    delete (DbKey k) = R.ask >>= \conn -> liftIO $ do
        let sql = "DELETE FROM " ++ tableName (undefined :: v) ++ " WHERE id=?"
        s <- D.prepare conn sql
        D.bind s [D.SQLInteger k]
        D.Done <- D.step s
        D.finalize s

instance SqlUpdateTable v => HasUpdateTable v (R.ReaderT D.Database IO) where
    update (DbKey k) fields = R.ask >>= \conn -> liftIO $ do
        let sql = "UPDATE " ++ tableName (undefined :: v) ++ " SET " ++
                  intercalate ", " (map (\x -> toSQLName x ++ "=?") fields) ++
                  " WHERE id=?"
        s <- D.prepare conn sql
        D.bind s $ map updateToSQLData fields ++ [D.SQLInteger k]
        D.Done <- D.step s
        D.finalize s

instance SqlTable Person where
    tableName _ = "Person"
    toSQLData (Person name age) =
        [D.SQLText name, D.SQLInteger $ fromIntegral age]
    fromSQLData [D.SQLText name, D.SQLInteger age] =
        Right $ Person name $ fromIntegral age
    fromSQLData x = Left $ "Invalid person fields: " ++ show x
    columnNames _ = ["name", "age"]

instance SqlUpdateTable Person where
    toSQLName (PersonName _) = "name"
    toSQLName (PersonAge _) = "age"
    updateToSQLData (PersonName n) = D.SQLText n
    updateToSQLData (PersonAge a) = D.SQLInteger $ fromIntegral a

main = do
    putStrLn "StateT Map"
    main1
    putStrLn "\n\nReaderT Database (sqlite)"
    main2

main1 = flip S.evalStateT (Map.empty :: Map.Map Int Person) $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print mp1
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print mp2
    update pid1 [PersonAge 27]
    mp3 <- get pid1
    liftIO $ print mp3
    replace pid1 $ Person "Michael" 28
    mp4 <- get pid1
    liftIO $ print mp4
    p5s <- select [PersonNameEq "Michael"]
    liftIO $ print p5s
    p6s <- select [PersonAgeLt 27]
    liftIO $ print p6s
    p7s <- select [PersonAgeLt 29]
    liftIO $ print p7s
    p8s <- select [PersonNameEq "Michael", PersonAgeLt 29]
    liftIO $ print p8s
    p9s <- select [PersonNameEq "Michael", PersonAgeLt 27]
    liftIO $ print p9s
    delete pid1
    mplast <- get pid1
    liftIO $ print mplast

main2 = do
  db <- D.open ":memory:"
  initDb db (undefined :: Person)
  flip R.runReaderT db $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print mp1
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print mp2
    update pid1 [PersonAge 27]
    mp3 <- get pid1
    liftIO $ print mp3
    replace pid1 $ Person "Michael" 28
    mp4 <- get pid1
    liftIO $ print mp4
    {- FIXME
    p5s <- select [PersonNameEq "Michael"]
    liftIO $ print p5s
    p6s <- select [PersonAgeLt 27]
    liftIO $ print p6s
    p7s <- select [PersonAgeLt 29]
    liftIO $ print p7s
    p8s <- select [PersonNameEq "Michael", PersonAgeLt 29]
    liftIO $ print p8s
    p9s <- select [PersonNameEq "Michael", PersonAgeLt 27]
    liftIO $ print p9s
    -}
    delete pid1
    mplast <- get pid1
    liftIO $ print mplast
