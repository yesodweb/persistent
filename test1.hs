{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding (filter)
import Database.Persist
import Database.Persist.State
import Database.Persist.Sqlite3
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Database.HDBC.Sqlite3 (connectSqlite3)

derivePersistSqlite3 $ Table "Person"
    [ ("name", "String")
    , ("age", "Int")
    ]
    ["name", "age"]
    [ ("name", True, True, False, False, False, False)
    , ("age", False, False, False, True, False, False)
    ]
    [ ("name", False, True)
    , ("age", True, False)
    ]
    [("PersonNameKey", ["name"])]

deriving instance Show Person

main = do
    --evalPersistState go (Map.empty :: Map.Map Int Person)
    conn <- connectSqlite3 "test.db3"
    runSqlite3 go conn

go = do
    initialize (undefined :: Person)
    pid <- insert $ Person "Michael" 25
    liftIO $ print pid

    p1 <- get pid
    liftIO $ print p1

    replace pid $ Person "Michael" 26
    p2 <- get pid
    liftIO $ print p2

    p3 <- filter [PersonNameEq "Michael"]
    liftIO $ print p3

    _ <- insert $ Person "Michael2" 27
    deleteWhere [PersonNameEq "Michael2"]
    p4 <- filter [PersonAgeLt 28]
    liftIO $ print p4

    update pid [PersonAge 28]
    p5 <- get pid
    liftIO $ print p5

    updateWhere [PersonNameEq "Michael"] [PersonAge 29]
    p6 <- get pid
    liftIO $ print p6

    insert $ Person "Eliezer" 2
    p7 <- order [PersonAgeAsc]
    liftIO $ print p7

    insert $ Person "Abe" 30
    p8 <- select [PersonAgeLt 30] [PersonNameDesc]
    liftIO $ print p8

    insertR $ Person "Abe" 31
    p9 <- filter [PersonNameEq "Abe"]
    liftIO $ print p9

    p10 <- getBy $ PersonNameKey "Michael"
    liftIO $ print p10

    delete pid
    plast <- get pid
    liftIO $ print plast
