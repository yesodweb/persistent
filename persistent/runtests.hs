{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Test.HUnit hiding (Test)
import Test.Hspec
import Test.Hspec.HUnit

import Database.Persist.GenericSql
import Database.Persist.Sqlite
import Database.Persist.Base (PersistUpdate (Add, Assign), PersistFilter (..), ColumnDef (ColumnDef), DeleteCascade (..))
import Database.Persist ((&&.), (||.))
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import Database.Persist.TH
import Control.Monad.IO.Class

import Database.Persist.Join hiding (RunJoin)
import qualified Database.Persist.Join
import qualified Database.Persist.Join.Sql

import Control.Monad.Trans.Reader
import Control.Monad (unless)
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
share [mkPersist,  mkMigrate "testMigrate", mkDeleteCascade] [$persist|

  Person
    name String
    age Int "some ignored -- attribute"
    color String Maybe -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
  Person1
    name String
    age Int
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
    name String
  Entry
    author AuthorId
    title String
|]

-- connstr = "user=test password=test host=localhost port=5432 dbname=yesod_test"

runConn f = do
    withSqlitePool "testdb" 1 $ runSqlPool f
#if WITH_POSTGRESQL
    withPostgresqlPool "user=test password=test host=localhost port=5432 dbname=test" 1 $ runSqlPool f
#endif

sqlTest :: SqlPersist IO () -> Assertion
sqlTest actions = do
  runConn $ actions >>= \r -> rollback >> return r


cleanDB :: SqlPersist IO ()
cleanDB = do
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Number])
  deleteWhere ([] :: [Filter Entry])
  deleteWhere ([] :: [Filter Author])

setup :: SqlPersist IO ()
setup = do
  runMigration testMigrate
  cleanDB

main :: IO ()
main = do
    runConn setup
    hspecX $ describe "persistent"
        [ it "passes the general tests" caseGeneral
        , it "deleteWhere" caseDeleteWhere
        , it "deleteBy" caseDeleteBy
        , it "delete" caseDelete
        , it "replace" caseReplace
        , it "getBy" caseGetBy
        , it "update" caseUpdate
        , it "updateWhere" caseUpdateWhere
        , it "selectList" caseSelectList
        , it "selectFirst" caseSelectFirst
        , it "large numbers" caseLargeNumbers
        , it "insertBy" caseInsertBy
        , it "derivePersistField" caseDerivePersistField
        , it "afterException" caseAfterException
        , it "idIn" caseIdIn
        , it "joinNonSql" caseJoinNonSql
        , it "joinSql" caseJoinSql
        , it "and/or" caseAndOr
        , it "commit/rollback" (caseCommitRollback >> runConn cleanDB)
        ]

assertEmpty xs    = liftIO $ assertBool "" (null xs)
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

caseDeleteWhere :: Assertion
caseDeleteWhere = sqlTest $ do
  key2 <- insert $ Person "Michael2" 90 Nothing
  _    <- insert $ Person "Michael3" 90 Nothing
  let p91 = Person "Michael4" 91 Nothing
  key91 <- insert $ p91

  ps90 <- selectList [PersonAge ==. 90] []
  assertNotEmpty ps90
  deleteWhere [PersonAge ==. 90]
  ps90 <- selectList [PersonAge ==. 90] []
  assertEmpty ps90
  Nothing <- get key2

  Just p2_91 <- get key91
  p91 @== p2_91

caseDeleteBy :: Assertion
caseDeleteBy = sqlTest $ do
  key2 <- insert $ Person "Michael2" 27 Nothing
  let p3 = Person "Michael3" 27 Nothing
  key3 <- insert $ p3

  ps2 <- selectList [PersonName ==. "Michael2"] []
  assertNotEmpty ps2

  deleteBy $ PersonNameKey "Michael2"
  ps2 <- selectList [PersonName ==. "Michael2"] []
  assertEmpty ps2

  Just p32 <- get key3
  p3 @== p32

caseDelete :: Assertion
caseDelete = sqlTest $ do
  key2 <- insert $ Person "Michael2" 27 Nothing
  let p3 = Person "Michael3" 27 Nothing
  key3 <- insert $ p3

  pm2 <- selectList [PersonName ==. "Michael2"] []
  assertNotEmpty pm2
  delete key2
  pm2 <- selectList [PersonName ==. "Michael2"] []
  assertEmpty pm2

  Just p <- get key3
  p3 @== p

-- also a decent test of get
caseReplace :: Assertion
caseReplace = sqlTest $ do
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

caseGetBy :: Assertion
caseGetBy = sqlTest $ do
  let p2 = Person "Michael2" 27 Nothing
  key2 <- insert p2
  Just (k, p) <- getBy $ PersonNameKey "Michael2"
  p @== p2
  k @== key2
  Nothing <- getBy $ PersonNameKey "Michael3"

  Just (k', p') <- getByValue p2
  k' @== k
  p' @== p
  return ()

caseAndOr :: Assertion
caseAndOr = sqlTest $ do
  deleteWhere ([] :: [Filter Person1])
  _ <- insert $ Person1 "Michael" 25
  _ <- insert $ Person1 "Miriam" 25
  _ <- insert $ Person1 "Michael" 30
  _ <- insert $ Person1 "Michael" 35

  c10 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"]
  c10 @== 4
  c12 <- count [FilterOr [Person1Name ==. "Michael", Person1Name ==. "Miriam"]]
  c12 @== 4
  c14 <- count [FilterOr [Person1Name ==. "Michael", Person1Name ==. "Miriam",
                            Person1Age >. 29, Person1Age <=. 30]]
  c14 @== 4

  c20 <- count $ [Person1Name ==. "Miriam"] ||. [Person1Age >. 29, Person1Age <=. 30]
  c20 @== 2
  c22 <- count $ [Person1Age <=. 30] &&. [Person1Age >. 29]
  c22 @== 1
  c24 <- count $ [FilterAnd [Person1Age <=. 30, Person1Age >. 29]]
  c24 @== 1
  c26 <- count $ [Person1Age <=. 30] ++ [Person1Age >. 29]
  c26 @== 1

  c34 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Mirieam"] &&.[Person1Age <.35]
  c34 @== 3
  c30 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"]) &&.[Person1Age <.35]
  c30 @== 4
  c36 <- count $ [Person1Name ==. "Michael"] ||. ([Person1Name ==. "Miriam"] &&.[Person1Age <.35])
  c36 @== 4

  c40 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"] ||. [Person1Age <.35])
  c40 @== 4

caseUpdate :: Assertion
caseUpdate = sqlTest $ do
  let p25 = Person "Michael" 25 Nothing
  key25 <- insert p25
  update key25 [PersonAge =. 28, PersonName =. "Updated"]
  Just pBlue28 <- get key25
  pBlue28 @== Person "Updated" 28 Nothing
  update key25 [PersonAge +. 2]
  Just pBlue30 <- get key25
  pBlue30 @== Person "Updated" 30 Nothing

caseUpdateWhere :: Assertion
caseUpdateWhere = sqlTest $ do
  let p1 = Person "Michael" 25 Nothing
  let p2 = Person "Michael2" 25 Nothing
  key1 <- insert p1
  key2 <- insert p2
  updateWhere [PersonName ==. "Michael2"]
              [PersonAge +. 3, PersonName =. "Updated"]
  Just pBlue28 <- get key2
  pBlue28 @== Person "Updated" 28 Nothing
  Just p <- get key1
  p @== p1

caseSelectList :: Assertion
caseSelectList = sqlTest $ do
  let p25 = Person "Michael" 25 Nothing
  let p26 = Person "Michael2" 26 Nothing
  key25 <- insert p25
  key26 <- insert p26
  ps <- selectList [] []
  ps @== [(key25, p25), (key26, p26)]
  -- limit
  ps <- selectList [] [LimitTo 1]
  ps @== [(key25, p25)]
  -- offset -- FAILS!
  ps <- selectList [] [OffsetBy 1]
  ps @== [(key26, p26)]
  -- limit & offset
  ps <- selectList [] [LimitTo 1, OffsetBy 1]
  ps @== [(key26, p26)]

  ps <- selectList [] [Desc PersonAge]
  ps @== [(key26, p26), (key25, p25)]
  ps <- selectList [PersonAge ==. 26] []
  ps @== [(key26, p26)]

caseSelectFirst :: Assertion
caseSelectFirst = sqlTest $ do
  _ <- insert $ Person "Michael" 26 Nothing
  let pOld = Person "Oldie" 75 Nothing
  kOld <- insert pOld

  x <- selectFirst [] [Desc PersonAge]
  x @== Just (kOld, pOld)

-- general tests transferred from already exising test file
caseGeneral :: Assertion
caseGeneral = sqlTest $ do
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
  personAge mic26 @== personAge mic + 1

  results <- selectList [PersonName ==. "Michael"] []
  results @== [(micK, mic26)]

  results <- selectList [PersonAge <. 28] []
  results @== [(micK, mic26)]

  update micK [PersonAge =. 28]
  Just p28 <- get micK
  personAge p28 @== 28

  updateWhere [PersonName ==. "Michael"] [PersonAge =. 29]
  Just mic29 <- get micK
  personAge mic29 @== 29

  let eli = Person "Eliezer" 2 $ Just "blue"
  _ <- insert eli
  pasc <- selectList [] [Asc PersonAge]
  (map snd pasc) @== [eli, mic29]

  let abe30 = Person "Abe" 30 $ Just "black"
  keyAbe30 <- insert abe30
  pdesc <- selectList [PersonAge <. 30] [Desc PersonName]
  (map snd pasc) @== [eli, mic29]

  abes <- selectList [PersonName ==. "Abe"] []
  (map snd abes) @== [abe30]

  Just (k,p) <- getBy $ PersonNameKey "Michael"
  p @== mic29

  ps <- selectList [PersonColor ==. Just "blue"] []
  map snd ps @== [eli]

  ps <- selectList [PersonColor ==. Nothing] []
  map snd ps @== [mic29]

  ps <- selectList [PersonColor /=. Nothing] []
  map snd ps @== [eli, abe30]

  delete micK
  Nothing <- get micK
  return ()

caseLargeNumbers :: Assertion
caseLargeNumbers = sqlTest $ do
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

caseInsertBy :: Assertion
caseInsertBy = sqlTest $ do
    Right _ <- insertBy $ Person "name" 1 Nothing
    Left _ <- insertBy $ Person "name" 1 Nothing
    Right _ <- insertBy $ Person "name2" 1 Nothing
    return ()

caseDerivePersistField :: Assertion
caseDerivePersistField = sqlTest $ do
    person <- insert $ Person "pet owner" 30 Nothing
    cat <- insert $ Pet person "Mittens" Cat
    Just cat' <- get cat
    liftIO $ petType cat' @?= Cat
    dog <- insert $ Pet person "Spike" Dog
    Just dog' <- get dog
    liftIO $ petType dog' @?= Dog

caseCommitRollback :: Assertion
caseCommitRollback = sqlTest $ do
    let filt :: [Filter Person1]
        filt = []

    let p = Person1 "foo" 0

    _ <- insert p
    _ <- insert p
    _ <- insert p

    c1 <- count filt
    c1 @== 3

    commit
    c2 <- count filt
    c2 @== 3

    _ <- insert p
    rollback
    c3 <- count filt
    c3 @== 3

    _ <- insert p
    commit
    _ <- insert p
    _ <- insert p
    rollback
    c4 <- count filt
    c4 @== 4

caseAfterException :: Assertion
caseAfterException = withSqlitePool "testdb" 1 $ runSqlPool $ do
    _ <- insert $ Person "A" 0 Nothing
    _ <- (insert (Person "A" 1 Nothing) >> return ()) `Control.catch` catcher
    _ <- insert $ Person "B" 0 Nothing
    return ()
  where
    catcher :: Monad m => SomeException -> m ()
    catcher _ = return ()

caseIdIn :: Assertion
caseIdIn = sqlTest $ do
    let p1 = Person "D" 0 Nothing
        p2 = Person "E" 1 Nothing
        p3 = Person "F" 2 Nothing
    pid1 <- insert p1
    _pid2 <- insert p2
    pid3 <- insert p3
    x <- selectList [PersonId <-. [pid1, pid3]] []
    liftIO $ x @?= [(pid1, p1), (pid3, p3)]

caseJoinNonSql :: Assertion
caseJoinNonSql = sqlTest $ _joinGen Database.Persist.Join.runJoin

caseJoinSql :: Assertion
caseJoinSql = sqlTest $ _joinGen Database.Persist.Join.Sql.runJoin

_joinGen run = do
    a <- insert $ Author "a"
    a1 <- insert $ Entry a "a1"
    a2 <- insert $ Entry a "a2"
    a3 <- insert $ Entry a "a3"
    b <- insert $ Author "b"
    b1 <- insert $ Entry b "b1"
    b2 <- insert $ Entry b "b2"
    c <- insert $ Author "c"

    x <- run $ selectOneMany (EntryAuthor <-.) entryAuthor
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

    y <- run $ (selectOneMany (EntryAuthor <-.) entryAuthor)
            { somFilterOne = [AuthorName ==. "a"]
            }
    liftIO $
        y @?=
            [ ((a, Author "a"),
                [ (a1, Entry a "a1")
                , (a2, Entry a "a2")
                , (a3, Entry a "a3")
                ])
            ]

    z <- run (selectOneMany (EntryAuthor <-.) entryAuthor)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
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

    w <- run (selectOneMany (EntryAuthor <-.) entryAuthor)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
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
