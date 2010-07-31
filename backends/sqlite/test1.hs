{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Prelude hiding (filter)
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Database.Persist.Pool

mkPersist [$persist|
Person sql=PersonTable
    name String update Eq Ne Desc
    age Int update "Asc" Lt "some ignored attribute"
    color String null Eq Ne sql=mycolorfield
    PersonNameKey name
Pet
    owner PersonId
    name String
|]

main :: IO ()
main = withSqlitePool "test.db3" 1 $ runSqlPool go

go :: SqlPersist IO ()
go = do
    runMigration $ do
        migrate (undefined :: Person)
        migrate (undefined :: Pet)
    deleteWhere ([] :: [Filter Person])

    pid <- insert $ Person "Michael" 25 Nothing
    liftIO $ print pid

    p1 <- get pid
    liftIO $ print p1

    replace pid $ Person "Michael" 26 Nothing
    p2 <- get pid
    liftIO $ print p2

    p3 <- selectList [PersonNameEq "Michael"] [] 0 0
    liftIO $ print p3

    _ <- insert $ Person "Michael2" 27 Nothing
    deleteWhere [PersonNameEq "Michael2"]
    p4 <- selectList [PersonAgeLt 28] [] 0 0
    liftIO $ print p4

    update pid [PersonAge 28]
    p5 <- get pid
    liftIO $ print p5

    updateWhere [PersonNameEq "Michael"] [PersonAge 29]
    p6 <- get pid
    if fmap personAge p6 /= Just 29 then error "bug 57" else return ()
    liftIO $ print p6

    _ <- insert $ Person "Eliezer" 2 $ Just "blue"
    p7 <- selectList [] [PersonAgeAsc] 0 0
    liftIO $ print p7

    _ <- insert $ Person "Abe" 30 $ Just "black"
    p8 <- selectList [PersonAgeLt 30] [PersonNameDesc] 0 0
    liftIO $ print p8

    {-
    insertR $ Person "Abe" 31 $ Just "brown"
    p9 <- select [PersonNameEq "Abe"] []
    liftIO $ print p9
    -}

    p10 <- getBy $ PersonNameKey "Michael"
    liftIO $ print p10

    p11 <- selectList [PersonColorEq $ Just "blue"] [] 0 0
    liftIO $ print p11

    p12 <- selectList [PersonColorEq Nothing] [] 0 0
    liftIO $ print p12

    p13 <- selectList [PersonColorNe Nothing] [] 0 0
    liftIO $ print p13

    p14 <- count [PersonColorNe Nothing]
    liftIO $ print p14

    delete pid
    plast <- get pid
    liftIO $ print plast

    _ <- insert $ Person "Gavriella" 0 Nothing
    x@(_, Person "Gavriella" 0 Nothing) <-
        insertBy $ Person "Gavriella" 1 $ Just "blue"
    liftIO $ print x

    False <- checkUnique $ Person "Gavriella" 2 Nothing
    True <- checkUnique $ Person "Gavriela (it's misspelled)" 2 Nothing
    return ()
