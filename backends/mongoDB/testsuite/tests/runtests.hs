{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck
import Test.HUnit hiding (Test)

import Database.Persist.MongoDB
import Database.Persist.TH
import Control.Monad.IO.Class
import Control.Monad.Context
import Control.Exception
import Control.Monad.Throw

import Database.Persist.Join hiding (RunJoin)
import qualified Database.Persist.Join
import qualified Database.Persist.Join.Sql
import qualified Database.MongoDB as DB
import Database.MongoDB.Connection

import Control.Monad.Trans.Reader
import Monad (unless)
import Data.Int
import Data.Word

import Control.Exception (SomeException)
import qualified Control.Exception.Control as Control


infix 1 /=@, @/=

(/=@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
expected /=@ actual = liftIO $ assertNotEqual "" expected actual

(@/=) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
actual @/= expected = liftIO $ assertNotEqual "" expected actual

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

infix 1 @==, ==@
expected @== actual = liftIO $ expected @?= actual
expected ==@ actual = liftIO $ expected @=? actual

data PetType = Cat | Dog
    deriving (Show, Read, Eq)
derivePersistField "PetType"

  -- FIXME Empty
share2 mkPersist (mkMigrate "testMigrate") [$persist|

  Person
    name String Update Eq Ne Desc
    age Int Update "Asc" Desc Lt "some ignored -- attribute" Eq Add
    color String Maybe Eq Ne -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
  Pet
    owner PersonId
    name String
    type PetType
  NeedsPet
    pet PetId
  Number
    int Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64

  Author
    name String Eq Asc
  Entry
    author AuthorId In
    title String Desc
|]

-- connstr = "user=test password=test host=localhost port=5432 dbname=yesod_test"

runConn f = do
    withMongoDBConn "test" "127.0.0.1" $ runMongoDBConn f

-- TODO: run tests in transaction
mongoTest :: MongoDBReader Database.MongoDB.Connection.Host IO () -> Assertion
mongoTest actions = do
  runConn actions
  runConn cleanDB

cleanDB :: MongoDBReader Database.MongoDB.Connection.Host IO () 
cleanDB = do
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Number])
  deleteWhere ([] :: [Filter Entry])
  deleteWhere ([] :: [Filter Author])

setup :: MongoDBReader Database.MongoDB.Connection.Host IO () 
setup = do
  --runMigration testMigrate
  cleanDB

main :: IO ()
main = do
  runConn setup
  defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Database.Persistent" $ reverse
    [ 
      testCase "sqlite persistent" case_sqlitePersistent
    , testCase "sqlite deleteWhere" case_sqliteDeleteWhere
    , testCase "sqlite deleteBy" case_sqliteDeleteBy
    , testCase "sqlite delete" case_sqliteDelete
    , testCase "sqlite replace" case_sqliteReplace
    , testCase "sqlite getBy" case_sqliteGetBy
    , testCase "sqlite update" case_sqliteUpdate
    , testCase "sqlite updateWhere" case_sqliteUpdateWhere
    , testCase "sqlite selectList" case_sqliteSelectList
    , testCase "large numbers" case_largeNumbers
    , testCase "insertBy" case_insertBy
    , testCase "derivePersistField" case_derivePersistField
    , testCase "idIn" case_idIn
    , testCase "join (non-SQL)" case_joinNonSql
 --   , testCase "join (SQL)" case_joinSql
 --   , testCase "afterException" case_afterException
    ]

                          
assertEmpty xs    = liftIO $ assertBool "" (null xs)
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

case_sqliteDeleteWhere = mongoTest _deleteWhere
case_sqliteDeleteBy = mongoTest _deleteBy
case_sqliteDelete = mongoTest _delete
case_sqliteReplace = mongoTest _replace
case_sqliteGetBy = mongoTest _getBy
case_sqliteUpdate = mongoTest _update
case_sqliteUpdateWhere = mongoTest _updateWhere
case_sqliteSelectList = mongoTest _selectList
case_sqlitePersistent = mongoTest _persistent
case_largeNumbers = mongoTest _largeNumbers
case_insertBy = mongoTest _insertBy
case_derivePersistField = mongoTest _derivePersistField
--case_afterException = mongoTest _afterException
case_idIn = mongoTest _idIn
case_joinNonSql = mongoTest _joinNonSql
--case_joinSql = mongoTest _joinSql

_deleteWhere = do
  key2 <- insert $ Person "Michael2" 90 Nothing
  _    <- insert $ Person "Michael3" 90 Nothing
  let p91 = Person "Michael4" 91 Nothing
  key91 <- insert $ p91

  ps90 <- selectList [PersonAgeEq 90] [] 0 0
  assertNotEmpty ps90
  deleteWhere [PersonAgeEq 90]
  ps90 <- selectList [PersonAgeEq 90] [] 0 0
  assertEmpty ps90
  Nothing <- get key2

  Just p2_91 <- get key91
  p91 @== p2_91

_deleteBy = do
  key2 <- insert $ Person "Michael2" 27 Nothing
  let p3 = Person "Michael3" 27 Nothing
  key3 <- insert $ p3

  ps2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertNotEmpty ps2

  deleteBy $ PersonNameKey "Michael2"
  ps2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertEmpty ps2

  Just p32 <- get key3
  p3 @== p32

_delete = do
  key2 <- insert $ Person "Michael2" 27 Nothing
  let p3 = Person "Michael3" 27 Nothing
  key3 <- insert $ p3

  pm2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertNotEmpty pm2
  delete key2
  pm2 <- selectList [PersonNameEq "Michael2"] [] 0 0
  assertEmpty pm2

  Just p <- get key3
  p3 @== p

-- also a decent test of get
_replace = do
  key2 <- insert $ Person "Michael2" 27 Nothing
  let p3 = Person "Michael3" 27 Nothing
  replace key2 p3
  Just p <- get key2
  p @== p3

  -- test replace an empty key
  delete key2
  Nothing <- get key2
  undefined <- replace key2 p3
  Nothing <- get key2
  return ()

_getBy = do
  let p2 = Person "Michael2" 27 Nothing
  key2 <- insert p2
  Just (k, p) <- getBy $ PersonNameKey "Michael2"
  p @== p2
  k @== key2
  Nothing <- getBy $ PersonNameKey "Michael3"
  return ()

_update = do
  let p25 = Person "Michael" 25 Nothing
  key25 <- insert p25
  update key25 [PersonAge 28, PersonName "Updated"]
  Just pBlue28 <- get key25
  pBlue28 @== Person "Updated" 28 Nothing
  update key25 [PersonAgeAdd 2]
  Just pBlue30 <- get key25
  pBlue30 @== Person "Updated" 30 Nothing

_updateWhere = do
  let p1 = Person "Michael" 25 Nothing
  let p2 = Person "Michael2" 25 Nothing
  key1 <- insert p1
  key2 <- insert p2
  updateWhere [PersonNameEq "Michael2"]
              [PersonName "Updated", PersonAgeAdd 3]
  Just pBlue28 <- get key2
  pBlue28 @== Person "Updated" 28 Nothing
  Just p <- get key1
  p @== p1

_selectList = do
  let p25 = Person "Michael" 25 Nothing
  let p26 = Person "Michael2" 26 Nothing
  key25 <- insert p25
  key26 <- insert p26
  ps <- selectList [] [] 0 0
  ps @== [(key25, p25), (key26, p26)]
  -- limit
  ps <- selectList [] [] 1 0
  ps @== [(key25, p25)]
  -- offset -- FAILS!
  ps <- selectList [] [] 0 1
  ps @== [(key26, p26)]
  -- limit & offset
  ps <- selectList [] [] 1 1
  ps @== [(key26, p26)]

  ps <- selectList [] [PersonAgeDesc] 0 0
  ps @== [(key26, p26), (key25, p25)]
  ps <- selectList [PersonAgeEq 26] [] 0 0
  ps @== [(key26, p26)]

-- general tests transferred from already exising test file
_persistent = do
  let mic = Person "Michael" 25 Nothing
  micK <- insert mic
  Just p <- get micK
  p @== mic

  replace micK $ Person "Michael" 25 Nothing
  Just p <- get micK
  p @== mic

  replace micK $ Person "Michael" 26 Nothing
  Just mic26 <- get micK
  mic26 @/= mic
  personAge mic26 @== (personAge mic) + 1

  results <- selectList [PersonNameEq "Michael"] [] 0 0
  results @== [(micK, mic26)]

  results <- selectList [PersonAgeLt 28] [] 0 0
  results @== [(micK, mic26)]

  update micK [PersonAge 28]
  Just p28 <- get micK
  personAge p28 @== 28

  updateWhere [PersonNameEq "Michael"] [PersonAge 29]
  Just mic29 <- get micK
  personAge mic29 @== 29

  let eli = Person "Eliezer" 2 $ Just "blue"
  _ <- insert eli
  pasc <- selectList [] [PersonAgeAsc] 0 0
  (map snd pasc) @== [eli, mic29]

  let abe30 = Person "Abe" 30 $ Just "black"
  keyAbe30 <- insert abe30
  pdesc <- selectList [PersonAgeLt 30] [PersonNameDesc] 0 0
  (map snd pasc) @== [eli, mic29]

  abes <- selectList [PersonNameEq "Abe"] [] 0 0
  (map snd abes) @== [abe30]

  Just (k,p) <- getBy $ PersonNameKey "Michael"
  p @== mic29

  ps <- selectList [PersonColorEq $ Just "blue"] [] 0 0
  map snd ps @== [eli]

  ps <- selectList [PersonColorEq Nothing] [] 0 0
  map snd ps @== [mic29]

  ps <- selectList [PersonColorNe Nothing] [] 0 0
  map snd ps @== [eli, abe30]

  delete micK
  Nothing <- get micK
  return ()

_largeNumbers = do
    go $ Number maxBound 0 0 0 0
    go $ Number 0 maxBound 0 0 0
    go $ Number 0 0 maxBound 0 0
    go $ Number 0 0 0 maxBound 0
    go $ Number 0 0 0 0 maxBound

    go $ Number minBound 0 0 0 0
    go $ Number 0 minBound 0 0 0
    go $ Number 0 0 minBound 0 0
    go $ Number 0 0 0 minBound 0
    go $ Number 0 0 0 0 minBound
  where
    go x = do
        xid <- insert x
        x' <- get xid
        liftIO $ x' @?= Just x

_insertBy = do
    Right _ <- insertBy $ Person "name" 1 Nothing
    Left _ <- insertBy $ Person "name" 1 Nothing
    Right _ <- insertBy $ Person "name2" 1 Nothing
    return ()

_derivePersistField = do
    person <- insert $ Person "pet owner" 30 Nothing
    cat <- insert $ Pet person "Mittens" Cat
    Just cat' <- get cat
    liftIO $ petType cat' @?= Cat
    dog <- insert $ Pet person "Spike" Dog
    Just dog' <- get dog
    liftIO $ petType dog' @?= Dog

_afterException = do
    _ <- insert $ Person "A" 0 Nothing
    _ <- (insert (Person "A" 1 Nothing) >> return ()) `Control.catch` catcher
    _ <- insert $ Person "B" 0 Nothing
    return ()
  where
    catcher :: Monad m => SomeException -> m ()
    catcher _ = return ()

_idIn = do
    let p1 = Person "A" 0 Nothing
        p2 = Person "B" 1 Nothing
        p3 = Person "C" 2 Nothing
    pid1 <- insert p1
    _pid2 <- insert p2
    pid3 <- insert p3
    x <- selectList [PersonIdIn [pid1, pid3]] [] 0 0
    liftIO $ x @?= [(pid1, p1), (pid3, p3)]

_joinNonSql = _joinGen Database.Persist.Join.runJoin
--_joinSql = _joinGen Database.Persist.Join.Sql.runJoin

_joinGen run = do
    a <- insert $ Author "a"
    a1 <- insert $ Entry a "a1"
    a2 <- insert $ Entry a "a2"
    a3 <- insert $ Entry a "a3"
    b <- insert $ Author "b"
    b1 <- insert $ Entry b "b1"
    b2 <- insert $ Entry b "b2"
    c <- insert $ Author "c"

    x <- run $ selectOneMany EntryAuthorIn entryAuthor
    liftIO $
        x @?=
            [ ((a, Author "a"),
                [ (a1, Entry a "a1")
                , (a2, Entry a "a2")
                , (a3, Entry a "a3")
                ])
            , ((b, Author "b"),
                [ (b1, Entry b "b1")
                , (b2, Entry b "b2")
                ])
            ]

    y <- run $ (selectOneMany EntryAuthorIn entryAuthor)
            { somFilterOne = [AuthorNameEq "a"]
            }
    liftIO $
        y @?=
            [ ((a, Author "a"),
                [ (a1, Entry a "a1")
                , (a2, Entry a "a2")
                , (a3, Entry a "a3")
                ])
            ]

    z <- run (selectOneMany EntryAuthorIn entryAuthor)
            { somOrderOne = [AuthorNameAsc]
            , somOrderMany = [EntryTitleDesc]
            }
    liftIO $
        z @?=
            [ ((a, Author "a"),
                [ (a3, Entry a "a3")
                , (a2, Entry a "a2")
                , (a1, Entry a "a1")
                ])
            , ((b, Author "b"),
                [ (b2, Entry b "b2")
                , (b1, Entry b "b1")
                ])
            ]

    w <- run (selectOneMany EntryAuthorIn entryAuthor)
            { somOrderOne = [AuthorNameAsc]
            , somOrderMany = [EntryTitleDesc]
            , somIncludeNoMatch = True
            }
    liftIO $
        w @?=
            [ ((a, Author "a"),
                [ (a3, Entry a "a3")
                , (a2, Entry a "a2")
                , (a1, Entry a "a1")
                ])
            , ((b, Author "b"),
                [ (b2, Entry b "b2")
                , (b1, Entry b "b1")
                ])
            , ((c, Author "c"), [])
            ]


instance Context DB.MasterOrSlaveOk IO where
  context = undefined
  push = undefined
instance Context DB.Database IO where
  context = undefined
  push = undefined
instance Context DB.WriteMode IO where
  context = undefined
  push = undefined
instance Context DB.Pipe IO where
  context = undefined
  push = undefined
instance Control.Monad.Throw.Throw DB.Failure IO where
  throw = undefined
  catch = undefined


