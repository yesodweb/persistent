{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Persist.Quasi
import Database.Persist.TH
import Prelude hiding (filter)
import Database.Persist
import Database.Persist.MongoDB
import qualified Network.Abstract(NetworkIO)
import Control.Monad.Context
import Control.Monad.IO.Class
import Control.Exception
import qualified Data.Map as Map
import qualified Database.MongoDB as DB
import Control.Monad.Throw
import Data.Maybe 

mkPersist [persist|
Person 
    name String Update Eq Ne Desc In
    age Int Update "Asc" Lt "some ignored attribute"
    color String Maybe Eq Ne NotIn Ge
    PersonNameKey name
Pet
    owner PersonId
    name String
Null
    field Int Maybe Eq Ne Gt NotIn In
Table
    table String
|]

main :: IO ()
main = withMongoDBConn "test" "127.0.0.1" $ runMongoDBConn go

go :: MongoDBReader DB.Host IO ()
go = do
    deleteWhere ([] :: [Filter Person])
  
    liftIO $ putStrLn "-- Test 1 --"
    pid <- insert $ Person "Michael" 25 Nothing
    (Just p) <- get pid
    --key <- return $ assert (Key p == pid) $ Key k
    liftIO $ print pid

    liftIO $ putStrLn "-- Test 3 --"
    replace pid $ Person "Michael" 26 Nothing
    p2 <- get pid
    liftIO $ print p2

    liftIO $ putStrLn "-- Test 4 --"
    p3 <- selectList [PersonNameEq "Michael"] [] 0 0
    liftIO $ print p3

    liftIO $ putStrLn "-- Test 5 --"
    _ <- insert $ Person "Michael2" 27 Nothing
    deleteWhere [PersonNameEq "Michael2"]
    p4 <- selectList [PersonAgeLt 28] [] 0 0
    liftIO $ print p4

    liftIO $ putStrLn "-- Test 6 --"
    update pid [PersonAge 28]
    p5 <- get pid
    liftIO $ print p5

    liftIO $ putStrLn "-- Test 7 --"
    updateWhere [PersonNameEq "Michael"] [PersonAge 29]
    p6 <- get pid
    if fmap personAge p6 /= Just 29 then error "bug 57" else return ()
    liftIO $ print p6

    liftIO $ putStrLn "-- Test 7 --"
    _ <- insert $ Person "Eliezer" 2 $ Just "blue"
    p7 <- selectList [] [PersonAgeAsc] 0 0
    liftIO $ print p7

    liftIO $ putStrLn "-- Test 8 --"
    _ <- insert $ Person "Abe" 30 $ Just "black"
    p8 <- selectList [PersonAgeLt 30] [PersonNameDesc] 0 0
    liftIO $ print p8

    {-
    insertR $ Person "Abe" 31 $ Just "brown"
    p9 <- select [PersonNameEq "Abe"] []
    liftIO $ print p9
    -}

    liftIO $ putStrLn "-- Test 10 --"
    p10 <- getBy $ PersonNameKey "Michael"
    liftIO $ print p10

    liftIO $ putStrLn "-- Test 11 --"
    p11 <- selectList [PersonColorEq $ Just "blue"] [] 0 0
    liftIO $ print p11

    liftIO $ putStrLn "-- Test 12 --"
    p12 <- selectList [PersonColorEq Nothing] [] 0 0
    liftIO $ print p12

    liftIO $ putStrLn "-- Test 13 --"
    p13 <- selectList [PersonColorNe Nothing] [] 0 0
    liftIO $ print p13

    liftIO $ putStrLn "-- Test 14 --"
    p14 <- count [PersonColorNe Nothing]
    liftIO $ print p14

    liftIO $ putStrLn "-- Test 15 --"
    delete pid
    plast <- get pid
    liftIO $ print plast

    liftIO $ putStrLn "-- Test 16 --"
    _ <- insert $ Person "Gavriella" 0 Nothing

--    x@(_, Person "Gavriella" 0 Nothing) <-
--        insertBy $ Person "Gavriella" 1 $ Just "blue"
--    liftIO $ print x

    liftIO $ putStrLn "-- Test 17 --"
    False <- checkUnique $ Person "Gavriella" 2 Nothing
    True <- checkUnique $ Person "Gavriela (it's misspelled)" 2 Nothing
    return ()

    liftIO $ putStrLn "-- Test 18 --"
    p15 <- selectList [PersonNameIn $ words "Michael Gavriella"] [] 0 0
    liftIO $ print p15

    liftIO $ putStrLn "-- Test 19 --"
    _ <- insert $ Person "Miriam" 23 $ Just "red"
    p16 <- selectList [PersonColorNotIn [Nothing, Just "blue"]] [] 0 0
    liftIO $ print p16

    liftIO $ putStrLn "-- Test 20 --"
    p17 <- selectList [PersonColorGe "blue"] [] 0 0
    liftIO $ print p17

    liftIO $ putStrLn "-- Test 21 --"
    deleteWhere ([] :: [Filter Null])
    _ <- insert $ Null $ Just 5
    _ <- insert $ Null Nothing
    [(_, Null (Just 5))] <- selectList [NullFieldGt 4] [] 0 0
    [] <- selectList [NullFieldGt 5] [] 0 0
    [(_, Null (Just 5))] <- selectList [NullFieldEq $ Just 5] [] 0 0
    [(_, Null Nothing)] <- selectList [NullFieldNe $ Just 5] [] 0 0
    [(_, Null Nothing)] <- selectList [NullFieldEq Nothing] [] 0 0
    [(_, Null (Just 5))] <- selectList [NullFieldNe Nothing] [] 0 0
    return ()

    _ <- selectList ([] :: [Filter Person]) [] 0 0
    _ <- selectList ([] :: [Filter Person]) [] 10 0
    _ <- selectList ([] :: [Filter Person]) [] 10 10
    _ <- selectList ([] :: [Filter Person]) [] 0 10

    deleteWhere ([] :: [Filter Table])
    _ <- insert $ Table "foo"
    _ <- insert $ Table "bar"

    return ()
