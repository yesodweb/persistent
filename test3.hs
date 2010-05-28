{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
import Database.Persist.QuasiYaml
import Prelude hiding (filter)
import Database.Persist
import Database.Persist.State
import Database.Persist.Sqlite3
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Database.HDBC.Sqlite3 (connectSqlite3)

derivePersistSqlite3 [$persist|
name: Person
columns:
    name:
        type: String
        update: True
        filter: [Eq, Ne]
        order: [Desc]
    age:
        type: Int
        update: True
        filter: [Gt]
        order: [Asc]
    color:
        type: String
        nullable: True
        filter: [Eq, Ne]
uniques:
    PersonNameKey: [name]
|]

deriving instance Show Person

main = do
    --evalPersistState go (Map.empty :: Map.Map Int Person)
    conn <- connectSqlite3 "test.db3"
    runSqlite3 go conn

go = do
    initialize (undefined :: Person)
    pid <- insert $ Person "Michael" 25 Nothing
    liftIO $ print pid

    p1 <- get pid
    liftIO $ print p1

    replace pid $ Person "Michael" 26 Nothing
    p2 <- get pid
    liftIO $ print p2

    p3 <- filter [PersonNameEq "Michael"]
    liftIO $ print p3

    _ <- insert $ Person "Michael2" 27 Nothing
    deleteWhere [PersonNameEq "Michael2"]
    p4 <- filter [PersonAgeLt 28]
    liftIO $ print p4

    update pid [PersonAge 28]
    p5 <- get pid
    liftIO $ print p5

    updateWhere [PersonNameEq "Michael"] [PersonAge 29]
    p6 <- get pid
    liftIO $ print p6

    insert $ Person "Eliezer" 2 $ Just "blue"
    p7 <- order [PersonAgeAsc]
    liftIO $ print p7

    insert $ Person "Abe" 30 $ Just "black"
    p8 <- select [PersonAgeLt 30] [PersonNameDesc]
    liftIO $ print p8

    insertR $ Person "Abe" 31 $ Just "brown"
    p9 <- filter [PersonNameEq "Abe"]
    liftIO $ print p9

    p10 <- getBy $ PersonNameKey "Michael"
    liftIO $ print p10

    p11 <- filter [PersonColorEq $ Just "blue"]
    liftIO $ print p11

    p12 <- filter [PersonColorEq Nothing]
    liftIO $ print p12

    p13 <- filter [PersonColorNe Nothing]
    liftIO $ print p13

    delete pid
    plast <- get pid
    liftIO $ print plast
