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
module PersistentTest where

import Test.HUnit hiding (Test)
import Test.Hspec.HUnit()
import Test.Hspec.QuickCheck(prop)

import Database.Persist
import Database.Persist.Query.Internal

#ifdef WITH_MONGODB
import Database.Persist.MongoDB (Action, oidToKey)
import Data.Bson (genObjectId)
import Language.Haskell.TH.Syntax (Type(..))

#else
import Database.Persist.Store (PersistValue( PersistInt64 ))
import Database.Persist.TH (mkDeleteCascade)
import Database.Persist.EntityDef (EntityDef(..), DBName(..))
import Database.Persist.Store ( DeleteCascade (..) )
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal (escapeName)
import Database.Persist.Sqlite
import Control.Exception (SomeException)
import Control.Monad.Trans.Reader (ask)
import qualified Data.Text as T
#  if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Exception as E
#    define CATCH catch'
#  else
import qualified Control.Exception.Control as Control
#    define CATCH Control.catch
#  endif
import System.Random

#  if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
#  if WITH_MYSQL
import Database.Persist.MySQL()
#  endif

#endif

import Database.Persist.TH (derivePersistField, persistUpperCase)
import Control.Monad.IO.Class

#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control
#else
import qualified Control.Monad.IO.Control
#endif

import Data.Text (Text)
import Web.PathPieces (PathPiece (..))
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M
import Init
import Data.Aeson

data PetType = Cat | Dog
    deriving (Show, Read, Eq)
derivePersistField "PetType"

#ifdef WITH_MONGODB
mkPersist MkPersistSettings { mpsBackend = ConT ''Action } [persistUpperCase|
#else
share [mkPersist sqlSettings,  mkMigrate "testMigrate", mkDeleteCascade] [persistUpperCase|
#endif

-- Dedented comment
  -- Header-level comment
    -- Indented comment
  Person json
    name String
    age Int "some ignored -- \" attribute"
    color String Maybe -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
    deriving Show Eq
  Person1
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    name String
    age Int
  Pet no-json
    ownerId PersonId
    name String
    deriving Show Eq
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    type PetType
  MaybeOwnedPet no-json
    ownerId PersonId Maybe
    name String
    type PetType
-- Dedented comment
  -- Header-level comment
    -- Indented comment
  NeedsPet
    petKey PetId

  -- From the scaffold
  User
    ident Text
    password Text Maybe
    UniqueUser ident
  Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email
|]
cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter MaybeOwnedPet])
  deleteWhere ([] :: [Filter NeedsPet])
  deleteWhere ([] :: [Filter User])
  deleteWhere ([] :: [Filter Email])
#ifdef WITH_MONGODB
db = db' cleanDB
#endif

petOwner :: PersistStore b m => PetGeneric b -> b m (PersonGeneric b)
petOwner = belongsToJust petOwnerId

maybeOwnedPetOwner :: PersistStore b m => MaybeOwnedPetGeneric b -> b m (Maybe (PersonGeneric b))
maybeOwnedPetOwner = belongsTo maybeOwnedPetOwnerId



specs :: Specs
specs = describe "persistent" $ do
  it "FilterOr []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p 
      ps <- selectList [FilterOr []] [Desc PersonAge]
      assertEmpty ps

  it "FilterAnd []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
      ps <- selectList [FilterAnd []] [Desc PersonAge]
      assertNotEmpty ps

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

      a <- fmap (map $ personName . entityVal) $ selectList [] [Desc PersonAge, Asc PersonName, OffsetBy 2, LimitTo 3]
      a @== ["y", "v", "x"]

      b <- fmap (map $ personName . entityVal) $ selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName]
      b @== a

      c <- fmap (map $ personName . entityVal) $ selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName, LimitTo 1, OffsetBy 1]
      c @== a


  it "passes the general tests" $ db $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26 
      results <- selectList [PersonName ==. "Michael"] []
      results @== [(Entity micK mic26)]

      results' <- selectList [PersonAge <. 28] []
      results' @== [(Entity micK mic26)]

      update micK [PersonAge =. 28]
      Just p28 <- get micK
      personAge p28 @== 28

      updateWhere [PersonName ==. "Michael"] [PersonAge =. 29]
      Just mic29 <- get micK
      personAge mic29 @== 29

      let eli = Person "Eliezer" 2 $ Just "blue"
      _ <- insert eli
      pasc <- selectList [] [Asc PersonAge]
      (map entityVal pasc) @== [eli, mic29]

      let abe30 = Person "Abe" 30 $ Just "black"
      _ <- insert abe30
      -- pdesc <- selectList [PersonAge <. 30] [Desc PersonName]
      (map entityVal pasc) @== [eli, mic29]

      abes <- selectList [PersonName ==. "Abe"] []
      (map entityVal abes) @== [abe30]

      Just (Entity _ p3) <- getBy $ PersonNameKey "Michael"
      p3 @== mic29

      ps <- selectList [PersonColor ==. Just "blue"] []
      map entityVal ps @== [eli]

      ps2 <- selectList [PersonColor ==. Nothing] []
      map entityVal ps2 @== [mic29]

      delete micK
      Nothing <- get micK
      return ()

  it "persistIdField" $ db $ do
      let p = Person "foo" 100 (Just "blue")
          q = Person "bar" 101 Nothing
      pk <- insert p
      qk <- insert q

      mp <- selectFirst [persistIdField ==. pk] []
      fmap entityVal mp @== Just p

      mq <- selectFirst [persistIdField ==. qk] []
      fmap entityVal mq @== Just q

  it "!=." $ db $ do
      deleteWhere ([] :: [Filter Person])
      let mic = Person "Michael" 25 Nothing
      _ <- insert mic
      let eli = Person "Eliezer" 25 (Just "Red")
      _ <- insert eli

      pne <- selectList [PersonName !=. "Michael"] []
      map entityVal pne @== [eli]

      ps <- selectList [PersonColor !=. Nothing] []
      map entityVal ps @== [eli]

      pnm <- selectList [PersonName !=. "Eliezer"] []
      map entityVal pnm @== [mic]


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
      Just (Entity k p) <- getBy $ PersonNameKey "Michael2"
      p @== p2
      k @== key2
      Nothing <- getBy $ PersonNameKey "Michael9"

      Just (Entity k' p') <- getByValue p2
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
      ps1 @== [(Entity key25 p25), (Entity key26 p26)]
      -- limit
      ps2 <- selectList [] [LimitTo 1]
      ps2 @== [(Entity key25 p25)]
      -- offset
      ps3 <- selectList [] [OffsetBy 1]
      ps3 @== [(Entity key26 p26)]
      -- limit & offset
      ps4 <- selectList [] [LimitTo 1, OffsetBy 1]
      ps4 @== [(Entity key26 p26)]

      ps5 <- selectList [] [Desc PersonAge]
      ps5 @== [(Entity key26 p26), (Entity key25 p25)]
      ps6 <- selectList [PersonAge ==. 26] []
      ps6 @== [(Entity key26 p26)]


  it "selectFirst" $ db $ do
      _ <- insert $ Person "Michael" 26 Nothing
      let pOld = Person "Oldie" 75 Nothing
      kOld <- insert pOld

      x <- selectFirst [] [Desc PersonAge]
      x @== Just (Entity kOld pOld)


  it "insertBy" $ db $ do
      Right _ <- insertBy $ Person "name" 1 Nothing
      Left _ <- insertBy $ Person "name" 1 Nothing
      Right _ <- insertBy $ Person "name2" 1 Nothing
      return ()

  it "insertKey" $ db $ do
#ifdef WITH_MONGODB
      oid <- liftIO $ genObjectId
      let k = oidToKey oid
#else
      ki <- liftIO $ randomRIO (0, 10000)
      let k = Key $ PersistInt64 $ abs ki
#endif
      insertKey k $ Person "Key" 26 Nothing
      Just (Entity k2 _) <- selectFirst [PersonName ==. "Key"] []
      k2 @== k

  it "repsert" $ db $ do
#ifdef WITH_MONGODB
      oid <- liftIO $ genObjectId
      let k = oidToKey oid
#else
      ki <- liftIO $ randomRIO (0, 10000)
      let k = Key $ PersistInt64 $ abs ki
#endif
      Nothing <- selectFirst [PersonName ==. "Repsert"] []
      repsert k $ Person "Repsert" 26 Nothing
      Just (Entity k2 _) <- selectFirst [PersonName ==. "Repsert"] []
      k2 @== k
      repsert k $ Person "Repsert" 27 Nothing
      Just (Entity k3 p) <- selectFirst [PersonName ==. "Repsert"] []
      k3 @== k
      27 @== personAge p

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
      liftIO $ x @?= [(Entity pid1 p1), (Entity pid3 p3)]

  it "serializes to JSON" $ do
      toJSON (Person "D" 0 Nothing) @?=
        Object (M.fromList [("color",Null),("name",String "D"),("age",Number 0)])
    


#ifndef WITH_MONGODB
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
      escape <- ((. DBName) . escapeName) `fmap` SqlPersist ask
      let query = T.concat [ "SELECT ??, ?? "
                           , "FROM ", escape "Person"
                           , ", ", escape "Pet"
                           , " WHERE ", escape "Person", ".", escape "age", " >= ? "
                           , "AND ", escape "Pet", ".", escape "ownerId", " = "
                                   , escape "Person", ".", escape "id"
                           , " ORDER BY ", escape "Person", ".", escape "name"
                           ]
      ret <- rawSql query [PersistInt64 20]
      liftIO $ ret @?= [ (Entity p1k p1, Entity a1k a1)
                       , (Entity p1k p1, Entity a2k a2)
                       , (Entity p2k p2, Entity a3k a3) ]

  it "rawSql/order-proof" $ db $ do
      let p1 = Person "Zacarias" 93 Nothing
      p1k <- insert p1
      escape <- ((. DBName) . escapeName) `fmap` SqlPersist ask
      let query = T.concat [ "SELECT ?? "
                           , "FROM ", escape "Person"
                           ]
      ret1 <- rawSql query []
      ret2 <- rawSql query []
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
    type PersistEntityBackend (ReverseFieldOrder a) = PersistEntityBackend a
    persistIdField = error "ReverseFieldOrder.persistIdField"

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

-- Test proper polymorphism
_polymorphic :: PersistQuery b m => b m ()
_polymorphic = do
    ((Entity id' _):_) <- selectList [] [LimitTo 1]
    _ <- selectList [PetOwnerId ==. id'] []
    _ <- insert $ Pet id' "foo" Cat
    return ()
