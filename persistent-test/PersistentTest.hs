{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module PersistentTest
    ( specs
    , runConn
    , setup
    ) where

import Test.HUnit hiding (Test)
import Test.Hspec.Monadic (Specs, describe, it)
import Test.Hspec.HUnit()
import Test.Hspec.QuickCheck(prop)

import Database.Persist
import Database.Persist.Store (PersistValue(..))
import Database.Persist.Query

import Database.Persist.Query.Join (selectOneMany, SelectOneMany(..))
import qualified Database.Persist.Query.Join

#if WITH_MONGODB
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (Action, withMongoDBConn, runMongoDBConn, oidToKey)
import Data.Bson (genObjectId)
import Language.Haskell.TH.Syntax (Type(..))
import Database.Persist.TH (MkPersistSettings(..))
import Control.Monad (replicateM)
import qualified Data.ByteString as BS

#else
import Database.Persist.EntityDef (EntityDef(..))
import Database.Persist.Store ( DeleteCascade (..) )
import Database.Persist.GenericSql
import qualified Database.Persist.Query.Join.Sql
import Database.Persist.Sqlite
import Control.Exception (SomeException)
#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Exception as E
#define CATCH catch'
#else
import qualified Control.Exception.Control as Control
#define CATCH Control.catch
#endif
import System.Random

import Control.Monad.Trans.Resource (ResourceIO)

#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif

#endif

import Database.Persist.TH (mkPersist, mkMigrate, derivePersistField, share, sqlMkSettings, persistUpperCase, mkDeleteCascade)
import Control.Monad.IO.Class

import Control.Monad (unless)
import Data.Int
import Data.Word
#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control
#else
import qualified Control.Monad.IO.Control
#endif

import Data.Text (Text)
import Web.PathPieces (PathPiece (..))
import Data.Maybe (fromJust)
import Test.QuickCheck

{-
expected /=@ actual = liftIO $ assertNotEqual "" expected actual
-}
(@/=), (@==), (==@) :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
infix 1 @/= --, /=@
actual @/= expected = liftIO $ assertNotEqual "" expected actual

infix 1 @==, ==@
expected @== actual = liftIO $ expected @?= actual
expected ==@ actual = liftIO $ expected @=? actual

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

assertEmpty :: (Monad m, MonadIO m) => [a] -> m ()
assertEmpty xs    = liftIO $ assertBool "" (null xs)

assertNotEmpty :: (Monad m, MonadIO m) => [a] -> m ()
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))


data PetType = Cat | Dog
    deriving (Show, Read, Eq)
derivePersistField "PetType"

#if WITH_MONGODB
mkPersist MkPersistSettings { mpsBackend = ConT ''Action } [persistSQL|
#else
share [mkPersist sqlMkSettings,  mkMigrate "testMigrate", mkDeleteCascade] [persistUpperCase|
#endif
-- Dedented comment
  -- Header-level comment
    -- Indented comment
  Person
    name String
    age Int "some ignored -- \" attribute"
    color String Maybe -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
  Person1
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    name String
    age Int
  Pet
    ownerId PersonId
    name String
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    type PetType
  MaybeOwnedPet
    ownerId PersonId Maybe
    name String
    type PetType
-- Dedented comment
  -- Header-level comment
    -- Indented comment
  NeedsPet
    petKey PetId
  Number
    int Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64

  Author
    name String
  Entry
    authorId AuthorId
    title String
|]

petOwner :: PersistStore b m => PetGeneric b -> b m Person
petOwner = belongsToJust petOwnerId

maybeOwnedPetOwner :: PersistStore b m => MaybeOwnedPetGeneric b -> b m (Maybe Person)
maybeOwnedPetOwner = belongsTo maybeOwnedPetOwnerId

-- this is faster then dropDatabase. Could try dropCollection
cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Number])
  deleteWhere ([] :: [Filter Entry])
  deleteWhere ([] :: [Filter Author])

#ifdef WITH_MONGODB
type BackendMonad = Action
runConn f = do
--    withMongoDBConn ("test") "127.0.0.1" $ runMongoDBConn MongoDB.safe MongoDB.Master f
  withMongoDBConn "test" "127.0.0.1" $ runMongoDBConn MongoDB.master f

--setup :: MongoPersist IO ()
setup :: Action IO ()
setup = do
  -- TODO: check version
  -- v <- MongoDB.access MongoDB.pipe MongoDB.slaveOk "admin" $ MongoDB.runCommand1 "buildInfo"
  v <- MongoDB.serverVersion
  liftIO $ putStrLn $ "version: " ++ show v
  if andVersion v then return () else error "mongoDB version not supported: need at least 1.9.1"
  -- TODO: use dropDatabase
  MongoDB.dropDatabase "test"   --(MongoDB.Database "test")
  return ()
  where
    andVersion vresult = case show vresult of
      '"':'1':'.':n:'.':minor -> let i = ((read [n]) ::Int) in i > 9 || (i == 9 && ((read $ init minor)::Int) >= 1)
      '"':'2':'.':_ -> True

--db :: MongoPersist IO () -> Assertion
db :: Action IO () -> Assertion
db actions = do
  r <- runConn actions
  runConn cleanDB
  return r

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack `fmap` replicateM 12 arbitrary

instance Arbitrary PersistValue where
    arbitrary = PersistObjectId `fmap` arbitrary

#else
type BackendMonad = SqlPersist
sqlite_database :: Text
sqlite_database = "test/testdb.sqlite3"
runConn :: ResourceIO m => SqlPersist m t -> m ()
runConn f = do
    _<-withSqlitePool sqlite_database 1 $ runSqlPool f
#if WITH_POSTGRESQL
    _<-withPostgresqlPool "user=test password=test host=localhost port=5432 dbname=test" 1 $ runSqlPool f
#endif
    return ()

db :: SqlPersist IO () -> Assertion
db actions = do
  runConn $ actions >>= \r -> rollback >> return r

setup :: SqlPersist IO ()
setup = do
  printMigration testMigrate
  runMigrationUnsafe testMigrate
  cleanDB

instance Random Int32 where
    random g =
        let ((i::Int), g') = random g in
        (fromInteger $ toInteger i, g')
    randomR (lo, hi) g =
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Random Int64 where
    random g =
        let ((i0::Int32), g0) = random g
            ((i1::Int32), g1) = random g0 in
        (fromInteger (toInteger i0) + fromInteger (toInteger i1) * 2 ^ (32::Int), g1)
    randomR (lo, hi) g = -- TODO : generate on the whole range, and not only on a part of it
        let ((i::Int), g') = randomR (fromInteger $ toInteger lo, fromInteger $ toInteger hi) g in
        (fromInteger $ toInteger i, g')

instance Arbitrary PersistValue where
    arbitrary = PersistInt64 `fmap` choose (0, maxBound)
#endif



joinGeneric :: PersistQuery b m =>
               (SelectOneMany BackendMonad (Author) (EntryGeneric BackendMonad)
                -> b m [((Key b (Author), Author),
                                 [(Key b (EntryGeneric b),
                                   EntryGeneric b)])])
                -> b m ()

joinGeneric run = do
    a <- insert $ Author "a"
    a1 <- insert $ Entry a "a1"
    a2 <- insert $ Entry a "a2"
    a3 <- insert $ Entry a "a3"
    b <- insert $ Author "b"
    b1 <- insert $ Entry b "b1"
    b2 <- insert $ Entry b "b2"
    c <- insert $ Author "c"

    x <- run $ selectOneMany (EntryAuthorId <-.) entryAuthorId
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

    y <- run $ (selectOneMany (EntryAuthorId <-.) entryAuthorId)
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

    z <- run (selectOneMany (EntryAuthorId <-.) entryAuthorId)
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

    w <- run (selectOneMany (EntryAuthorId <-.) entryAuthorId)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
            , somIncludeNoMatch = True
            }
    liftIO $
        w @==
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
  it "FilterOr []" $ db $ do
      _ <- selectList [FilterOr []] [Desc PersonAge]
      return ()
  it "FilterAnd []" $ db $ do
      _ <- selectList [FilterAnd []] [Desc PersonAge]
      return ()
  it "order of opts is irrelevant" $ db $ do
      let eq (a, b, _) (c, d) = (a, b) @== (c, d)
      limitOffsetOrder [Desc PersonAge] `eq` (0, 0)
      limitOffsetOrder [LimitTo 2, Desc PersonAge] `eq` (2, 0)
      limitOffsetOrder [Desc PersonAge, LimitTo 2] `eq` (2, 0)
      limitOffsetOrder [LimitTo 2, Desc PersonAge, OffsetBy 3] `eq` (2, 3)

      _ <- insert $ Person "z" 1 Nothing
      _ <- insert $ Person "y" 2 Nothing
      _ <- insert $ Person "x" 1 Nothing
      _ <- insert $ Person "w" 2 Nothing
      _ <- insert $ Person "v" 1 Nothing
      _ <- insert $ Person "u" 2 Nothing

      a <- fmap (map $ personName . snd) $ selectList [] [Desc PersonAge, Asc PersonName, OffsetBy 2, LimitTo 3]
      a @== ["y", "v", "x"]

      b <- fmap (map $ personName . snd) $ selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName]
      b @== a

      c <- fmap (map $ personName . snd) $ selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName, LimitTo 1, OffsetBy 1]
      c @== a

  it "passes the general tests" $ db $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26 
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

      delete micK
      Nothing <- get micK
      return ()


  it "!=." $ db $ do
      deleteWhere ([] :: [Filter Person])
      let mic = Person "Michael" 25 Nothing
      _ <- insert mic
      let eli = Person "Eliezer" 25 (Just "Red")
      _ <- insert eli

      pne <- selectList [PersonName !=. "Michael"] []
      map snd pne @== [eli]

      ps <- selectList [PersonColor !=. Nothing] []
      map snd ps @== [eli]

      pnm <- selectList [PersonName !=. "Eliezer"] []
      map snd pnm @== [mic]


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

  prop "toPathPiece - fromPathPiece" $ \piece ->
      let key1 = Key piece :: (Key BackendMonad Person)
          key2 = fromJust $ fromPathPiece $ toPathPiece key1 :: (Key BackendMonad Person)
      in  toPathPiece key1 == toPathPiece key2

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

      let mic = Person "Michael" 25 Nothing
      micK <- insert mic
      Just p1 <- get micK
      p1 @== mic

      replace micK $ Person "Michael" 25 Nothing
      Just p2 <- get micK
      p2 @== mic

      replace micK $ Person "Michael" 26 Nothing
      Just mic26 <- get micK
      mic26 @/= mic
      personAge mic26 @== personAge mic + 1



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

#ifdef WITH_MONGODB
  it "insertKey" $ db $ do
      oid <- liftIO $ genObjectId
      let k = oidToKey oid
      insertKey k $ Person "Key" 26 Nothing
      Just (k2,_) <- selectFirst [PersonName ==. "Key"] []
      k2 @== k

  it "repsert" $ db $ do
      oid <- liftIO $ genObjectId
      let k = oidToKey oid
      Nothing <- selectFirst [PersonName ==. "Repsert"] []
      repsert k $ Person "Repsert" 26 Nothing
      Just (k2,_) <- selectFirst [PersonName ==. "Repsert"] []
      k2 @== k
      repsert k $ Person "Repsert" 27 Nothing
      Just (k3,p) <- selectFirst [PersonName ==. "Repsert"] []
      k3 @== k
      27 @== personAge p
#endif

  it "retrieves a belongsToJust association" $ db $ do
      let p = Person "pet owner" 30 Nothing
      person <- insert $ p
      let cat = Pet person "Mittens" Cat
      p2 <- getJust $ petOwnerId cat
      p @== p2
      p3 <- petOwner cat
      p @== p3

  it "retrieves a belongsTo association" $ db $ do
      let p = Person "pet owner" 30 Nothing
      person <- insert $ p
      let cat = MaybeOwnedPet (Just person) "Mittens" Cat
      p2 <- getJust $ fromJust $ maybeOwnedPetOwnerId cat
      p @== p2
      Just p4 <- maybeOwnedPetOwner cat
      p @== p4

  it "derivePersistField" $ db $ do
      person <- insert $ Person "pet owner" 30 Nothing
      catKey <- insert $ Pet person "Mittens" Cat
      Just cat' <- get catKey
      liftIO $ petType cat' @?= Cat
      dog <- insert $ Pet person "Spike" Dog
      Just dog' <- get dog
      liftIO $ petType dog' @?= Dog


  it "idIn" $ db $ do
      let p1 = Person "D" 0 Nothing
          p2 = Person "E" 1 Nothing
          p3 = Person "F" 2 Nothing
      pid1 <- insert p1
      _ <- insert p2
      pid3 <- insert p3
      x <- selectList [PersonId <-. [pid1, pid3]] []
      liftIO $ x @?= [(pid1, p1), (pid3, p3)]


  it "joinNonSql" $ db $ joinGeneric Database.Persist.Query.Join.runJoin

#ifndef WITH_MONGODB
  it "joinSql" $ db $ joinGeneric Database.Persist.Query.Join.Sql.runJoin

  it "rawSql/2+2" $ db $ do
      ret <- rawSql "SELECT 2+2" []
      liftIO $ ret @?= [Single (4::Int)]

  it "rawSql/?-?" $ db $ do
      ret <- rawSql "SELECT ?-?" [PersistInt64 5, PersistInt64 3]
      liftIO $ ret @?= [Single (2::Int)]

  it "rawSql/entity" $ db $ do
      let insert' :: (PersistStore b m, PersistEntity val) => val -> b m (Key b val, val)
          insert' v = insert v >>= \k -> return (k, v)
      (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
      (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
      (p3k, _ ) <- insert' $ Person "Cassandra" 19 Nothing
      (_  , _ ) <- insert' $ Person "Thiago"    19 Nothing
      (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
      (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
      (a3k, a3) <- insert' $ Pet p2k "Lhama"   Dog
      (_  , _ ) <- insert' $ Pet p3k "Abacate" Cat
      ret <- rawSql "SELECT ??, ?? FROM \"Person\", \"Pet\" WHERE \"Person\".age >= ? AND \"Pet\".\"ownerId\" = \"Person\".id ORDER BY \"Person\".name, \"Pet\".name" [PersistInt64 20]
      liftIO $ ret @?= [ (Entity p1k p1, Entity a1k a1)
                       , (Entity p1k p1, Entity a2k a2)
                       , (Entity p2k p2, Entity a3k a3) ]

  it "rawSql/order-proof" $ db $ do
      let p1 = Person "Zacarias" 93 Nothing
      p1k <- insert p1
      ret1 <- rawSql "SELECT ?? FROM \"Person\"" []
      ret2 <- rawSql "SELECT ?? FROM \"Person\"" []
      liftIO $ ret1 @?= [Entity p1k p1]
      liftIO $ ret2 @?= [Entity (Key $ unKey p1k) (RFO p1)]

  it "commit/rollback" (caseCommitRollback >> runConn cleanDB)

  it "afterException" caseAfterException

-- | Reverses the order of the fields of an entity.  Used to test
-- @??@ placeholders of 'rawSql'.
newtype ReverseFieldOrder a = RFO {unRFO :: a} deriving (Eq, Show)
instance PersistEntity a => PersistEntity (ReverseFieldOrder a) where
    newtype EntityField (ReverseFieldOrder a) b = EFRFO {unEFRFO :: EntityField a b}
    newtype Unique      (ReverseFieldOrder a) b = URFO  {unURFO  :: Unique      a b}
    persistFieldDef = persistFieldDef . unEFRFO
    entityDef = revFields . entityDef . unRFO
        where
          revFields ed = ed { entityFields = reverse (entityFields ed) }
    toPersistFields = reverse . toPersistFields . unRFO
    fromPersistValues = fmap RFO . fromPersistValues . reverse
    halfDefined = RFO halfDefined
    persistUniqueToFieldNames = reverse . persistUniqueToFieldNames . unURFO
    persistUniqueToValues = reverse . persistUniqueToValues . unURFO
    persistUniqueKeys = map URFO . reverse . persistUniqueKeys . unRFO


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

#if MIN_VERSION_monad_control(0, 3, 0)
catch' :: (Control.Monad.Trans.Control.MonadBaseControl IO m, E.Exception e)
       => m a       -- ^ The computation to run
       -> (e -> m a) -- ^ Handler to invoke if an exception is raised
       -> m a
catch' a handler = Control.Monad.Trans.Control.control $ \runInIO ->
                    E.catch (runInIO a)
                            (\e -> runInIO $ handler e)
#endif

caseAfterException :: Assertion
caseAfterException = withSqlitePool sqlite_database 1 $ runSqlPool $ do
    _ <- insert $ Person "A" 0 Nothing
    _ <- (insert (Person "A" 1 Nothing) >> return ()) `CATCH` catcher
    _ <- insert $ Person "B" 0 Nothing
    return ()
  where
    catcher :: Monad m => SomeException -> m ()
    catcher _ = return ()

#endif
