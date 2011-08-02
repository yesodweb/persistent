{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit

import Database.Persist.GenericSql
import Database.Persist
import Database.Persist.Base (PersistUpdate (Add, Assign), PersistFilter (..), ColumnDef (ColumnDef), DeleteCascade (..))

#if WITH_MONGODB
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (MongoPersist, withMongoDBConn, runMongoDBConn)
#else
import Control.Monad.Trans.Reader
import Database.Persist.Sqlite
import Control.Exception (SomeException)
import qualified Control.Exception.Control as Control
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
#endif

import Database.Persist.TH
import Control.Monad.IO.Class

import Database.Persist.Join hiding (RunJoin)
import qualified Database.Persist.Join
import qualified Database.Persist.Join.Sql

import Control.Monad (unless)
import Data.Int
import Data.Word

import ErrorLocation (debug)


infix 1 @/= --, /=@

{-
(/=@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
expected /=@ actual = liftIO $ assertNotEqual "" expected actual
-}

(@/=) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
actual @/= expected = liftIO $ assertNotEqual "" expected actual

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

assertEmpty xs    = liftIO $ assertBool "" (null xs)
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

infix 1 @== -- , ==@
expected @== actual = liftIO $ expected @?= actual
-- expected ==@ actual = liftIO $ expected @=? actual

data PetType = Cat | Dog
    deriving (Show, Read, Eq)
derivePersistField "PetType"

  -- FIXME Empty
share [mkPersist,  mkMigrate "testMigrate", mkDeleteCascade] [persist|

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

-- this is faster then dropDatabase. Could try dropCollection
cleanDB :: PersistBackend m => m ()
cleanDB = do
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Number])
  deleteWhere ([] :: [Filter Entry])
  deleteWhere ([] :: [Filter Author])

#ifdef WITH_MONGODB
runConn f = do
    withMongoDBConn (MongoDB.Database "test") "127.0.0.1" $ runMongoDBConn f MongoDB.safe MongoDB.Master

setup :: MongoPersist IO ()
setup = do
  -- TODO: check version
  -- v <- MongoDB.access MongoDB.pipe MongoDB.slaveOk "admin" $ MongoDB.runCommand1 "buildInfo"
  v <- MongoDB.serverVersion
  liftIO $ putStrLn $ "version: " ++ show v
  if andVersion v then return () else error "mongoDB version not supported: need at least 1.9.1"
  -- TODO: use dropDatabase 
  MongoDB.dropDatabase (MongoDB.Database "test")
  return ()
  where
    andVersion vresult = case debug $ show vresult of
      '"':'1':'.':n:'.':minor -> let i = ((read [n]) ::Int) in i > 9 || (i == 9 && ((read $ init minor)::Int) >= 1)
      '"':'2':'.':_ -> True

db :: MongoPersist IO () -> Assertion
db actions = do
  r <- runConn actions
  runConn cleanDB
  return r

#else
runConn f = do
    withSqlitePool "testdb" 1 $ runSqlPool f
#if WITH_POSTGRESQL
    withPostgresqlPool "user=test password=test host=localhost port=5432 dbname=test" 1 $ runSqlPool f
#endif

db :: SqlPersist IO () -> Assertion
db actions = do
  runConn $ actions >>= \r -> rollback >> return r

setup :: SqlPersist IO ()
setup = do
  runMigration testMigrate
  cleanDB
#endif

main :: IO ()
main = do
  runConn setup
  hspecX specs

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

specs :: Specs
specs = describe "persistent" $ do

  it "passes the general tests" $ db $ do
      let mic = Person "Michael" 25 Nothing
      micK <- insert mic
      Just p <- get micK
      p @== mic

      replace micK $ Person "Michael" 25 Nothing
      Just p2 <- get micK
      p2 @== mic

      replace micK $ Person "Michael" 26 Nothing
      Just mic26 <- get micK
      mic26 @/= mic
      personAge mic26 @== personAge mic + 1

      results <- selectList [PersonName ==. "Michael"] []
      results @== [(micK, mic26)]

      results' <- selectList [PersonAge <. 28] []
      results' @== [(micK, mic26)]

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
      _ <- insert abe30
      -- pdesc <- selectList [PersonAge <. 30] [Desc PersonName]
      (map snd pasc) @== [eli, mic29]

      abes <- selectList [PersonName ==. "Abe"] []
      (map snd abes) @== [abe30]

      Just (_,p3) <- getBy $ PersonNameKey "Michael"
      p3 @== mic29

      ps <- selectList [PersonColor ==. Just "blue"] []
      map snd ps @== [eli]

      ps2 <- selectList [PersonColor ==. Nothing] []
      map snd ps2 @== [mic29]

      ps3 <- selectList [PersonColor !=. Nothing] []
      map snd ps3 @== [eli, abe30]

      delete micK
      Nothing <- get micK
      return ()


  it "and/or" $ db $ do
      deleteWhere ([] :: [Filter Person1])
      _ <- insert $ Person1 "Michael" 25
      _ <- insert $ Person1 "Miriam" 25
      _ <- insert $ Person1 "Michael" 30
      _ <- insert $ Person1 "Michael" 35

      c10 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam", Person1Age ==. 25]
      c10 @== 4
      c12 <- count [FilterOr [FilterAnd [Person1Name ==. "Michael"], FilterAnd [Person1Name ==. "Miriam"]]]
      c12 @== 4
      c14 <- count [FilterOr [FilterAnd [Person1Name ==. "Michael"], FilterAnd [Person1Name ==. "Miriam"],
                              FilterAnd [Person1Age >. 29, Person1Age <=. 30]]]
      c14 @== 4

      c20 <- count $ [Person1Name ==. "Miriam"] ||. [Person1Age >. 29, Person1Age <=. 30]
      c20 @== 2
      c22 <- count $ [Person1Age <=. 30] ++ [Person1Age >. 29]
      c22 @== 1
      c24 <- count $ [FilterAnd [Person1Age <=. 30, Person1Age >. 29]]
      c24 @== 1
      c26 <- count $ [Person1Age <=. 30] ++ [Person1Age >. 29]
      c26 @== 1

      c34 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Mirieam"] ++ [Person1Age <.35]
      c34 @== 3
      c30 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"]) ++ [Person1Age <.35]
      c30 @== 3
      c36 <- count $ [Person1Name ==. "Michael"] ||. ([Person1Name ==. "Miriam"] ++ [Person1Age <.35])
      c36 @== 4

      c40 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"] ||. [Person1Age <.35])
      c40 @== 4


  it "deleteWhere" $ db $ do
      key2 <- insert $ Person "Michael2" 90 Nothing
      _    <- insert $ Person "Michael3" 90 Nothing
      let p91 = Person "Michael4" 91 Nothing
      key91 <- insert $ p91

      ps90 <- selectList [PersonAge ==. 90] []
      assertNotEmpty ps90
      deleteWhere [PersonAge ==. 90]
      ps90' <- selectList [PersonAge ==. 90] []
      assertEmpty ps90'
      Nothing <- get key2

      Just p2_91 <- get key91
      p91 @== p2_91


  it "deleteBy" $ db $ do
      _ <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      key3 <- insert $ p3

      ps2 <- selectList [PersonName ==. "Michael2"] []
      assertNotEmpty ps2

      deleteBy $ PersonNameKey "Michael2"
      ps2' <- selectList [PersonName ==. "Michael2"] []
      assertEmpty ps2'

      Just p32 <- get key3
      p3 @== p32


  it "delete" $ db $ do
      key2 <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      key3 <- insert $ p3

      pm2 <- selectList [PersonName ==. "Michael2"] []
      assertNotEmpty pm2
      delete key2
      pm2' <- selectList [PersonName ==. "Michael2"] []
      assertEmpty pm2'

      Just p <- get key3
      p3 @== p


  it "replace" $ db $ do
      key2 <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      replace key2 p3
      Just p <- get key2
      p @== p3

      -- test replace an empty key
      delete key2
      Nothing <- get key2
      _ <- replace key2 p3
      Nothing <- get key2
      return ()


  it "getBy" $ db $ do
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


  it "update" $ db $ do
      let p25 = Person "Michael" 25 Nothing
      key25 <- insert p25
      update key25 [PersonAge =. 28, PersonName =. "Updated"]
      Just pBlue28 <- get key25
      pBlue28 @== Person "Updated" 28 Nothing
      update key25 [PersonAge +=. 2]
      Just pBlue30 <- get key25
      pBlue30 @== Person "Updated" 30 Nothing


  it "updateWhere" $ db $ do
      let p1 = Person "Michael" 25 Nothing
      let p2 = Person "Michael2" 25 Nothing
      key1 <- insert p1
      key2 <- insert p2
      updateWhere [PersonName ==. "Michael2"]
                  [PersonAge +=. 3, PersonName =. "Updated"]
      Just pBlue28 <- get key2
      pBlue28 @== Person "Updated" 28 Nothing
      Just p <- get key1
      p @== p1


  it "selectList" $ db $ do
      let p25 = Person "Michael" 25 Nothing
      let p26 = Person "Michael2" 26 Nothing
      key25 <- insert p25
      key26 <- insert p26
      ps1 <- selectList [] []
      ps1 @== [(key25, p25), (key26, p26)]
      -- limit
      ps2 <- selectList [] [LimitTo 1]
      ps2 @== [(key25, p25)]
      -- offset -- FAILS!
      ps3 <- selectList [] [OffsetBy 1]
      ps3 @== [(key26, p26)]
      -- limit & offset
      ps4 <- selectList [] [LimitTo 1, OffsetBy 1]
      ps4 @== [(key26, p26)]

      ps5 <- selectList [] [Desc PersonAge]
      ps5 @== [(key26, p26), (key25, p25)]
      ps6 <- selectList [PersonAge ==. 26] []
      ps6 @== [(key26, p26)]


  it "selectFirst" $ db $ do
      _ <- insert $ Person "Michael" 26 Nothing
      let pOld = Person "Oldie" 75 Nothing
      kOld <- insert pOld

      x <- selectFirst [] [Desc PersonAge]
      x @== Just (kOld, pOld)

  it "large numbers" $ db $ do
      let go x = do
              xid <- insert x
              x' <- get xid
              liftIO $ x' @?= Just x

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


  it "insertBy" $ db $ do
      Right _ <- insertBy $ Person "name" 1 Nothing
      Left _ <- insertBy $ Person "name" 1 Nothing
      Right _ <- insertBy $ Person "name2" 1 Nothing
      return ()


  it "derivePersistField" $ db $ do
      person <- insert $ Person "pet owner" 30 Nothing
      cat <- insert $ Pet person "Mittens" Cat
      Just cat' <- get cat
      liftIO $ petType cat' @?= Cat
      dog <- insert $ Pet person "Spike" Dog
      Just dog' <- get dog
      liftIO $ petType dog' @?= Dog


  it "idIn" $ db $ do
      let p1 = Person "D" 0 Nothing
          p2 = Person "E" 1 Nothing
          p3 = Person "F" 2 Nothing
      pid1 <- insert p1
      _pid2 <- insert p2
      pid3 <- insert p3
      x <- selectList [PersonId <-. [pid1, pid3]] []
      liftIO $ x @?= [(pid1, p1), (pid3, p3)]


  it "joinNonSql" $ db $ _joinGen Database.Persist.Join.runJoin

#ifndef WITH_MONGODB
  it "joinSql" $ db $ _joinGen Database.Persist.Join.Sql.runJoin

  it "commit/rollback" (caseCommitRollback >> runConn cleanDB)

  it "afterException" caseAfterException

caseCommitRollback :: Assertion
caseCommitRollback = db $ do
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

#endif
