{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
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
import Control.Monad.Trans.Resource (runResourceT)
import Test.Hspec.Expectations ()
import Test.Hspec.QuickCheck(prop)

import Database.Persist

#ifdef WITH_NOSQL
#ifdef WITH_MONGODB
import qualified Database.MongoDB as MongoDB
import Database.Persist.MongoDB (toInsertDoc, docToEntityThrow, collectionName, recordToDocument)
#endif

#else

import Database.Persist.TH (mkDeleteCascade, mkSave)
import Control.Exception (SomeException)
import qualified Data.Text as T
#  if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Exception as E
#    define CATCH catch'
#  else
import qualified Control.Exception.Control as Control
#    define CATCH Control.catch
#  endif

#  ifdef WITH_POSTGRESQL
import Data.List (sort)
#  endif
#  if WITH_MYSQL
import Database.Persist.MySQL()
#  endif

#endif

#if MIN_VERSION_monad_control(0, 3, 0)
import qualified Control.Monad.Trans.Control
#else
import qualified Control.Monad.IO.Control
#endif
import Control.Exception.Lifted (catch)

import Control.Monad.IO.Class

import Web.PathPieces (PathPiece (..))
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M
import Init
import Data.Aeson

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Functor.Identity
import Data.Functor.Constant
import PersistTestPetType
import PersistTestPetCollarType

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist persistSettings,  mkMigrate "testMigrate", mkDeleteCascade persistSettings, mkSave "_ignoredSave"] [persistUpperCase|
#endif

-- Dedented comment
  -- Header-level comment
    -- Indented comment
  Person json
    name Text
    age Int "some ignored -- \" attribute"
    color Text Maybe -- this is a comment sql=foobarbaz
    PersonNameKey name -- this is a comment sql=foobarbaz
    deriving Show Eq
  Person1
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    name Text
    age Int
  PersonMaybeAge
    name Text
    age Int Maybe
  PersonMay json
    name Text Maybe
    color Text Maybe
    deriving Show Eq
  Pet
    ownerId PersonId
    name Text
    -- deriving Show Eq
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    type PetType
  MaybeOwnedPet
    ownerId PersonId Maybe
    name Text
    type PetType
-- Dedented comment
  -- Header-level comment
    -- Indented comment
  NeedsPet
    petKey PetId
  OutdoorPet
    ownerId PersonId
    collar PetCollar
    type PetType

  -- From the scaffold
  UserPT
    ident Text
    password Text Maybe
    UniqueUserPT ident
  EmailPT
    email Text
    user UserPTId Maybe
    verkey Text Maybe
    UniqueEmailPT email

  Upsert
    email Text
    counter Int
    UniqueUpsert email
    deriving Show

  UpsertBy
    email Text
    city Text
    state Text
    counter Int
    UniqueUpsertBy email
    UniqueUpsertByCityState city state
    deriving Show

  Strict
    !yes Int
    ~no Int
    def Int
|]

deriving instance Show (BackendKey backend) => Show (PetGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (PetGeneric backend)

share [mkPersist persistSettings { mpsPrefixFields = False, mpsGeneric = False }
#ifdef WITH_NOSQL
      ] [persistUpperCase|
#else
      , mkMigrate "noPrefixMigrate"
      ] [persistLowerCase|
#endif
NoPrefix1
    someFieldName Int
    deriving Show Eq
NoPrefix2
    someOtherFieldName Int
    unprefixedRef NoPrefix1Id
    deriving Show Eq
+NoPrefixSum
    unprefixedLeft Int
    unprefixedRight String
    deriving Show Eq
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend EmailPT ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter MaybeOwnedPet])
  deleteWhere ([] :: [Filter NeedsPet])
  deleteWhere ([] :: [Filter OutdoorPet])
  deleteWhere ([] :: [Filter UserPT])
  deleteWhere ([] :: [Filter EmailPT])

#ifdef WITH_NOSQL
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

catchPersistException :: Control.Monad.Trans.Control.MonadBaseControl IO m => m a -> b -> m b
catchPersistException action errValue = do
    Left res <-
      (Right `fmap` action) `catch`
      (\(_::PersistException) -> return $ Left errValue)
    return  res


specs :: Spec
specs = describe "persistent" $ do
  it "fieldLens" $ do
      let michael = Entity undefined $ Person "Michael" 28 Nothing :: Entity Person
          michaelP1 = Person "Michael" 29 Nothing :: Person
      view michael (fieldLens PersonAge) @?= 28
      entityVal (set (fieldLens PersonAge) 29 michael) @?= michaelP1

  it "FilterOr []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
#ifdef WITH_MONGODB
      ps <- catchPersistException (selectList [FilterOr []] [Desc PersonAge]) []
#else
      ps <- (selectList [FilterOr []] [Desc PersonAge])
#endif
      assertEmpty ps

  it "||. []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
#ifdef WITH_MONGODB
      c <- catchPersistException (count $ [PersonName ==. "a"] ||. []) 1
#else
      c <- (count $ [PersonName ==. "a"] ||. [])
#endif
      c @== (1::Int)

  it "FilterAnd []" $ db $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
      ps <- selectList [FilterAnd []] [Desc PersonAge]
      assertNotEmpty ps

  it "order of opts is irrelevant" $ db $ do
      let eq (a, b, _) (c, d) = (a, b) @== (c, d)
          limitOffsetOrder' :: [SelectOpt Person] -> (Int, Int, [SelectOpt Person])
          limitOffsetOrder' = limitOffsetOrder
      limitOffsetOrder' [Desc PersonAge] `eq` (0, 0)
      limitOffsetOrder' [LimitTo 2, Desc PersonAge] `eq` (2, 0)
      limitOffsetOrder' [Desc PersonAge, LimitTo 2] `eq` (2, 0)
      limitOffsetOrder' [LimitTo 2, Desc PersonAge, OffsetBy 3] `eq` (2, 3)

      insertMany_ [ Person "z" 1 Nothing
                  , Person "y" 2 Nothing
                  , Person "x" 1 Nothing
                  , Person "w" 2 Nothing
                  , Person "v" 1 Nothing
                  , Person "u" 2 Nothing
                  ]

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
      results @== [Entity micK mic26]

      results' <- selectList [PersonAge <. 28] []
      results' @== [Entity micK mic26]

      p28 <- updateGet micK [PersonAge =. 28]
      personAge p28 @== 28

#ifdef WITH_NOSQL
      updateWhere [PersonName ==. "Michael"] [PersonAge =. 29]
#else
      uc <- updateWhereCount [PersonName ==. "Michael"] [PersonAge =. 29]
      uc @== 1
#endif
      Just mic29 <- get micK
      personAge mic29 @== 29

      let eli = Person "Eliezer" 2 $ Just "blue"
      _ <- insert eli
      pasc <- selectList [] [Asc PersonAge]
      map entityVal pasc @== [eli, mic29]

      let abe30 = Person "Abe" 30 $ Just "black"
      _ <- insert abe30
      -- pdesc <- selectList [PersonAge <. 30] [Desc PersonName]
      map entityVal pasc @== [eli, mic29]

      abes <- selectList [PersonName ==. "Abe"] []
      map entityVal abes @== [abe30]

      Just (Entity _ p3) <- getBy $ PersonNameKey "Michael"
      p3 @== mic29

      ps <- selectList [PersonColor ==. Just "blue"] []
      map entityVal ps @== [eli]

      ps2 <- selectList [PersonColor ==. Nothing] []
      map entityVal ps2 @== [mic29]

      delete micK
      Nothing <- get micK
      return ()
#ifdef WITH_ZOOKEEPER
      -- zookeeper backend does not support idfield
      -- zookeeper's key is node-name.
      -- When uniq-key exists, zookeeper's key becomes encoded uniq-key.
#else
  it "persistIdField" $ db $ do
      let p = Person "foo" 100 (Just "blue")
          q = Person "bar" 101 Nothing
      pk <- insert p
      qk <- insert q

      mp <- selectFirst [persistIdField ==. pk] []
      fmap entityVal mp @== Just p

      mq <- selectFirst [persistIdField ==. qk] []
      fmap entityVal mq @== Just q
#endif

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

  it "Double Maybe" $ db $ do
      deleteWhere ([] :: [Filter PersonMay])
      let mic = PersonMay (Just "Michael") Nothing
      _ <- insert mic
      let eli = PersonMay (Just "Eliezer") (Just "Red")
      _ <- insert eli
      pe <- selectList [PersonMayName ==. Nothing, PersonMayColor ==. Nothing] []
      map entityVal pe @== []
      pne <- selectList [PersonMayName !=. Nothing, PersonMayColor !=. Nothing] []
      map entityVal pne @== [eli]

  it "and/or" $ db $ do
      deleteWhere ([] :: [Filter Person1])
      insertMany_ [ Person1 "Michael" 25
                  , Person1 "Miriam" 25
                  , Person1 "Michael" 30
                  , Person1 "Michael" 35
                  ]

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

#ifdef WITH_ZOOKEEPER
  it "toPathPiece . fromPathPiece" $  do
  --  Below quickcheck causes error of "Cannot convert PersistObjectId to Text."
  --  Currently, ZooKey does not support PersistObjectId.
      let key1 = ZooKey "hogehogekey" :: (BackendKey BackendMonad)
          key2 = fromJust $ fromPathPiece $ toPathPiece key1 :: (BackendKey BackendMonad)
      toPathPiece key1 `shouldBe` toPathPiece key2
#else
  prop "toPathPiece . fromPathPiece" $ \piece ->
      let key1 = piece :: (BackendKey BackendMonad)
          key2 = fromJust $ fromPathPiece $ toPathPiece key1 :: (BackendKey BackendMonad)
      in  toPathPiece key1 == toPathPiece key2
#endif

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


  it "updateGet" $ db $ do
      let p25 = Person "Michael" 25 Nothing
      key25 <- insert p25
      pBlue28 <- updateGet key25 [PersonAge =. 28, PersonName =. "Updated"]
      pBlue28 @== Person "Updated" 28 Nothing
      pBlue30 <- updateGet key25 [PersonAge +=. 2]
      pBlue30 @== Person "Updated" 30 Nothing

  it "upsert without updates" $ db $ do
      deleteWhere ([] :: [Filter Upsert])
      let email = "dude@example.com"
      Nothing :: Maybe (Entity Upsert) <- getBy $ UniqueUpsert email
      let counter1 = 0
      Entity k1 u1 <- upsert (Upsert email counter1) []
      upsertCounter u1 @== counter1
      let counter2 = 1
      Entity k2 u2 <- upsert (Upsert email counter2) []
      upsertCounter u2 @== counter2
      k1 @== k2

  it "upsert with updates" $ db $ do
      deleteWhere ([] :: [Filter Upsert])
      let email = "dude@example.com"
      Nothing :: Maybe (Entity Upsert) <- getBy $ UniqueUpsert email
      let up0 = Upsert email 0
      Entity _ up1 <- upsert up0 [UpsertCounter +=. 1]
      upsertCounter up1 @== 1
      Entity _ up2 <- upsert up1 [UpsertCounter +=. 1]
      upsertCounter up2 @== 2

  it "upsertBy without updates" $ db $ do
      deleteWhere ([] :: [Filter UpsertBy])
      let email = "dude@example.com"
          city = "Boston"
          state = "Massachussets"
      Nothing :: Maybe (Entity UpsertBy) <- getBy $ UniqueUpsertBy email
      let counter1 = 0
          unique = UniqueUpsertBy email
      Entity k1 u1 <- upsertBy unique (UpsertBy email city state counter1) []
      upsertByCounter u1 @== counter1
      let counter2 = 1
      Entity k2 u2 <- upsertBy unique (UpsertBy email city state counter2) []
      upsertByCounter u2 @== counter2
      k1 @== k2

  it "upsertBy with updates" $ db $ do
      deleteWhere ([] :: [Filter UpsertBy])
      let email = "dude@example.com"
          city = "Boston"
          state = "Massachussets"
      Nothing :: Maybe (Entity UpsertBy) <- getBy $ UniqueUpsertBy email
      let up0 = UpsertBy email city state 0
      Entity _ up1 <- upsertBy (UniqueUpsertBy email) up0 [UpsertByCounter +=. 1]
      upsertByCounter up1 @== 1
      Entity _ up2 <- upsertBy (UniqueUpsertBy email) up1 [UpsertByCounter +=. 1]
      upsertByCounter up2 @== 2

  it "maybe update" $ db $ do
      let noAge = PersonMaybeAge "Michael" Nothing
      keyNoAge <- insert noAge
      noAge2 <- updateGet keyNoAge [PersonMaybeAgeAge +=. Just 2]
      -- the correct answer is very debatable
#ifdef WITH_NOSQL
      personMaybeAgeAge noAge2 @== Just 2
#else
      personMaybeAgeAge noAge2 @== Nothing
#endif



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
      [key25, key26] <- insertMany [p25, p26]
      ps1 <- selectList [] [Asc PersonAge]
      ps1 @== [(Entity key25 p25), (Entity key26 p26)]
      -- limit
      ps2 <- selectList [] [Asc PersonAge, LimitTo 1]
      ps2 @== [(Entity key25 p25)]
      -- offset
      ps3 <- selectList [] [Asc PersonAge, OffsetBy 1]
      ps3 @== [(Entity key26 p26)]
      -- limit & offset
      ps4 <- selectList [] [Asc PersonAge, LimitTo 1, OffsetBy 1]
      ps4 @== [(Entity key26 p26)]

      ps5 <- selectList [] [Desc PersonAge]
      ps5 @== [(Entity key26 p26), (Entity key25 p25)]
      ps6 <- selectList [PersonAge ==. 26] []
      ps6 @== [(Entity key26 p26)]

  it "selectSource" $ db $ do
      let p1 = Person "selectSource1" 1 Nothing
          p2 = Person "selectSource2" 2 Nothing
          p3 = Person "selectSource3" 3 Nothing
      [k1,k2,k3] <- insertMany [p1, p2, p3]

      ps1 <- runResourceT $ selectSource [] [Desc PersonAge] $$ await
      ps1 @== Just (Entity k3 p3)

      ps2 <- runResourceT $ selectSource [PersonAge <. 3] [Asc PersonAge] $$ CL.consume
      ps2 @== [Entity k1 p1, Entity k2 p2]

      runResourceT $ selectSource [] [Desc PersonAge] $$ do
          e1 <- await
          e1 @== Just (Entity k3 p3)

          e2 <- await
          e2 @== Just (Entity k2 p2)

          e3 <- await
          e3 @== Just (Entity k1 p1)

          e4 <- await
          e4 @== Nothing

  it "selectFirst" $ db $ do
      _ <- insert $ Person "Michael" 26 Nothing
      let pOld = Person "Oldie" 75 Nothing
      kOld <- insert pOld

      x <- selectFirst [] [Desc PersonAge]
      x @== Just (Entity kOld pOld)


  it "selectKeys" $ db $ do
      let p1 = Person "selectKeys1" 1 Nothing
          p2 = Person "selectKeys2" 2 Nothing
          p3 = Person "selectKeys3" 3 Nothing
      [k1,k2,k3] <- insertMany [p1, p2, p3]

      ps1 <- runResourceT $ selectKeys [] [Desc PersonAge] $$ await
      ps1 @== Just k3

      ps2 <- runResourceT $ selectKeys [PersonAge <. 3] [Asc PersonAge] $$ CL.consume
      ps2 @== [k1, k2]

      runResourceT $ selectKeys [] [Desc PersonAge] $$ do
          e1 <- await
          e1 @== Just k3

          e2 <- await
          e2 @== Just k2

          e3 <- await
          e3 @== Just k1

          e4 <- await
          e4 @== Nothing

  it "insertMany_ with no arguments" $ db $ do
    _ <- insertMany_ ([] :: [Person])
    rows <- count ([] :: [Filter Person])
    rows @== 0
    _ <- insertMany ([] :: [Person])
    rows2 <- count ([] :: [Filter Person])
    rows2 @== 0
    _ <- insertEntityMany ([] :: [Entity Person])
    rows3 <- count ([] :: [Filter Person])
    rows3 @== 0

  it "insertEntityMany" $ db $ do
    id1:id2:id3:id4:id5:[] <- liftIO $ replicateM 5 (PersonKey `fmap` generateKey)
    let p1 = Entity id1 $ Person "insertEntityMany1" 1 Nothing
        p2 = Entity id2 $ Person "insertEntityMany2" 2 Nothing
        p3 = Entity id3 $ Person "insertEntityMany3" 3 Nothing
        p4 = Entity id4 $ Person "insertEntityMany4" 3 Nothing
        p5 = Entity id5 $ Person "insertEntityMany5" 3 Nothing
    insertEntityMany [p1,p2,p3,p4,p5]
    rows <- count ([] :: [Filter Person])
    rows @== 5

  it "insertBy" $ db $ do
      Right _ <- insertBy $ Person "name" 1 Nothing
      Left _ <- insertBy $ Person "name" 1 Nothing
      Right _ <- insertBy $ Person "name2" 1 Nothing
      return ()

  it "insertKey" $ db $ do
      k <- liftIO (PersonKey `fmap` generateKey)
      insertKey k $ Person "Key" 26 Nothing
      Just (Entity k2 _) <- selectFirst [PersonName ==. "Key"] []
      k2 @== k

  it "insertEntity" $ db $ do
      Entity k p <- insertEntity $ Person "name" 1 Nothing
      Just p2 <- get k
      p2 @== p

  it "repsert" $ db $ do
      k <- liftIO (PersonKey `fmap` generateKey)
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
      p3 <- belongsToJust petOwnerId $ cat
      p @== p3

  it "retrieves a belongsTo association" $ db $ do
      let p = Person "pet owner" 30 Nothing
      person <- insert p
      let cat = MaybeOwnedPet (Just person) "Mittens" Cat
      p2 <- getJust $ fromJust $ maybeOwnedPetOwnerId cat
      p @== p2
      Just p4 <- belongsTo maybeOwnedPetOwnerId $ cat
      p @== p4

  it "derivePersistField" $ db $ do
      person <- insert $ Person "pet owner" 30 Nothing
      catKey <- insert $ Pet person "Mittens" Cat
      Just cat' <- get catKey
      liftIO $ petType cat' @?= Cat
      dog <- insert $ Pet person "Spike" Dog
      Just dog' <- get dog
      liftIO $ petType dog' @?= Dog

  it "derivePersistFieldJSON" $ db $ do
      let mittensCollar = PetCollar "Mittens\n1-714-668-9672" True
      pkey <- insert $ Person "pet owner" 30 Nothing
      catKey <- insert $ OutdoorPet pkey mittensCollar Cat
      Just (OutdoorPet _ collar' _) <- get catKey
      liftIO $ collar' @?= mittensCollar

#ifdef WITH_ZOOKEEPER
      -- zookeeper backend does not support idfield
      -- zookeeper's key is node-name.
      -- When uniq-key exists, zookeeper's key becomes encoded uniq-key.
#else
  it "idIn" $ db $ do
      let p1 = Person "D" 0 Nothing
          p2 = Person "E" 1 Nothing
          p3 = Person "F" 2 Nothing
      pid1 <- insert p1
      _ <- insert p2
      pid3 <- insert p3
      x <- selectList [PersonId <-. [pid1, pid3]] []
      liftIO $ x @?= [Entity pid1 p1, Entity pid3 p3]
#endif

  describe "toJSON" $ do
    it "serializes" $ db $ do
      let p = Person "D" 0 Nothing
      k <- insert p
      liftIO $ toJSON (Entity k p) @?=
        Object (M.fromList [("id", toJSON k), ("color",Null),("name",String "D"),("age",Number 0)])

{- FIXME
    prop "fromJSON . toJSON $ key" $ \(person :: Key Person) ->
      case (fromJSON . toJSON) person of
        Success p -> p == person
        _ -> error "fromJSON"
        -}


#ifdef WITH_NOSQL
#ifdef WITH_MONGODB
  describe "raw MongoDB helpers" $ do
    it "collectionName" $ do
        collectionName (Person "Duder" 0 Nothing) @?= "Person"

    it "toInsertFields, entityFields, & docToEntityThrow" $ db $ do
        let p1 = Person "Duder" 0 Nothing
        let doc = toInsertDoc p1
        MongoDB.ObjId _id <- MongoDB.insert "Person" $ doc
        let idSelector = "_id" MongoDB.=: _id
        Entity _ ent1 <- docToEntityThrow $ idSelector:doc
        liftIO $ p1 @?= ent1

        let p2 = p1 {personColor = Just "blue"}
        let doc2 = idSelector:recordToDocument p2
        MongoDB.save "Person" doc2
        Entity _ ent2 <- docToEntityThrow doc2
        liftIO $ p2 @?= ent2
#endif
#else
  it "rawSql/2+2" $ db $ do
      ret <- rawSql "SELECT 2+2" []
      liftIO $ ret @?= [Single (4::Int)]

  it "rawSql/?-?" $ db $ do
      ret <- rawSql "SELECT ?-?" [PersistInt64 5, PersistInt64 3]
      liftIO $ ret @?= [Single (2::Int)]

  it "rawSql/NULL" $ db $ do
      ret <- rawSql "SELECT NULL" []
      liftIO $ ret @?= [Nothing :: Maybe (Single Int)]

  it "rawSql/entity" $ db $ do
      let insert' :: (PersistStore backend, PersistEntity val, PersistEntityBackend val ~ BaseBackend backend, MonadIO m)
                  => val -> ReaderT backend m (Key val, val)
          insert' v = insert v >>= \k -> return (k, v)
      (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
      (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
      (p3k, _ ) <- insert' $ Person "Cassandra" 19 Nothing
      (_  , _ ) <- insert' $ Person "Thiago"    19 Nothing
      (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
      (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
      (a3k, a3) <- insert' $ Pet p2k "Lhama"   Dog
      (_  , _ ) <- insert' $ Pet p3k "Abacate" Cat
      escape <- ((. DBName) . connEscapeName) `fmap` ask
      person <- getTableName (error "rawSql Person" :: Person)
      name   <- getFieldName PersonName
      let query = T.concat [ "SELECT ??, ?? "
                           , "FROM ", person
                           , ", ", escape "Pet"
                           , " WHERE ", person, ".", escape "age", " >= ? "
                           , "AND ", escape "Pet", ".", escape "ownerId", " = "
                                   , person, ".", escape "id"
                           , " ORDER BY ", person, ".", name
                           ]
      ret <- rawSql query [PersistInt64 20]
      liftIO $ ret @?= [ (Entity p1k p1, Entity a1k a1)
                       , (Entity p1k p1, Entity a2k a2)
                       , (Entity p2k p2, Entity a3k a3) ]
      ret2 <- rawSql query [PersistInt64 20]
      liftIO $ ret2 @?= [ (Just (Entity p1k p1), Just (Entity a1k a1))
                        , (Just (Entity p1k p1), Just (Entity a2k a2))
                        , (Just (Entity p2k p2), Just (Entity a3k a3)) ]
      ret3 <- rawSql query [PersistInt64 20]
      liftIO $ ret3 @?= [ Just (Entity p1k p1, Entity a1k a1)
                        , Just (Entity p1k p1, Entity a2k a2)
                        , Just (Entity p2k p2, Entity a3k a3) ]

  it "rawSql/order-proof" $ db $ do
      let p1 = Person "Zacarias" 93 Nothing
      p1k <- insert p1
      escape <- ((. DBName) . connEscapeName) `fmap` ask
      let query = T.concat [ "SELECT ?? "
                           , "FROM ", escape "Person"
                           ]
      ret1 <- rawSql query []
      ret2 <- rawSql query [] :: MonadIO m => SqlPersistT m [Entity (ReverseFieldOrder Person)]
      liftIO $ ret1 @?= [Entity p1k p1]
      liftIO $ ret2 @?= [Entity (RFOKey $ unPersonKey $ p1k) (RFO p1)]

  it "rawSql/OUTER JOIN" $ db $ do
      let insert' :: (PersistStore backend, PersistEntity val, PersistEntityBackend val ~ BaseBackend backend, MonadIO m)
                  => val -> ReaderT backend m (Key val, val)
          insert' v = insert v >>= \k -> return (k, v)
      (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
      (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
      (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
      (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
      escape <- ((. DBName) . connEscapeName) `fmap` ask
      let query = T.concat [ "SELECT ??, ?? "
                           , "FROM ", person
                           , "LEFT OUTER JOIN ", pet
                           , " ON ", person, ".", escape "id"
                           , " = ", pet, ".", escape "ownerId"
                           , " ORDER BY ", person, ".", escape "name"]
          person = escape "Person"
          pet    = escape "Pet"
      ret <- rawSql query []
      liftIO $ ret @?= [ (Entity p1k p1, Just (Entity a1k a1))
                       , (Entity p1k p1, Just (Entity a2k a2))
                       , (Entity p2k p2, Nothing) ]

  it "commit/rollback" (caseCommitRollback >> runResourceT (runConn cleanDB))

#ifndef WITH_MYSQL
#  ifndef WITH_POSTGRESQL
#    ifndef WITH_NOSQL
  it "afterException" $ db $ do
    let catcher :: Monad m => SomeException -> m ()
        catcher _ = return ()
    _ <- insert $ Person "A" 0 Nothing
    _ <- (insert (Person "A" 1 Nothing) >> return ()) `catch` catcher
    _ <- insert $ Person "B" 0 Nothing
    return ()
#    endif
#  endif
#endif


#ifndef WITH_NOSQL
  it "mpsNoPrefix" $ db $ do
    deleteWhere ([] :: [Filter NoPrefix2])
    deleteWhere ([] :: [Filter NoPrefix1])
    np1a <- insert $ NoPrefix1 1
    update np1a [SomeFieldName =. 2]
    np1b <- insert $ NoPrefix1 3
    np2 <- insert $ NoPrefix2 4 np1a
    update np2 [UnprefixedRef =. np1b, SomeOtherFieldName =. 5]

    mnp1a <- get np1a
    liftIO $ mnp1a @?= Just (NoPrefix1 2)
    liftIO $ fmap someFieldName mnp1a @?= Just 2
    mnp2 <- get np2
    liftIO $ fmap unprefixedRef mnp2 @?= Just np1b
    liftIO $ fmap someOtherFieldName mnp2 @?= Just 5

    insert_ $ UnprefixedLeftSum 5
    insert_ $ UnprefixedRightSum "Hello"

  it "IsSqlKey instance" $ db $ do
    let p = Person "Alice" 30 Nothing
    key@(PersonKey (SqlBackendKey i)) <- insert p
    liftIO $ fromSqlKey key `shouldBe` (i :: Int64)
    mp <- get $ toSqlKey i
    liftIO $ mp `shouldBe` Just p
#endif

  describe "strictness" $ do
    it "bang" $ (return $! Strict (error "foo") 5 5) `shouldThrow` anyErrorCall
    it "tilde" $ void (return $! Strict 5 (error "foo") 5 :: IO Strict)
    it "blank" $ (return $! Strict 5 5 (error "foo")) `shouldThrow` anyErrorCall

#ifdef WITH_POSTGRESQL
  describe "rawSql/array_agg" $ do
    let runArrayAggTest dbField expected = db $ do
          void $ insertMany
            [ UserPT "a" $ Just "b"
            , UserPT "c" $ Just "d"
            , UserPT "e"   Nothing
            , UserPT "g" $ Just "h" ]
          escape <- ((. DBName) . connEscapeName) `fmap` ask
          let query = T.concat [ "SELECT array_agg(", escape dbField, ") "
                               , "FROM ", escape "UserPT"
                               ]
          [Single xs] <- rawSql query []
          liftIO $ sort xs @?= expected

    it "works for [Text]"       $ runArrayAggTest "ident"    ["a", "c", "e", "g" :: Text]
    it "works for [Maybe Text]" $ runArrayAggTest "password" [Nothing, Just "b", Just "d", Just "h" :: Maybe Text]
#endif

-- | Reverses the order of the fields of an entity.  Used to test
-- @??@ placeholders of 'rawSql'.
newtype ReverseFieldOrder a = RFO {unRFO :: a} deriving (Eq, Show)
instance ToJSON (Key (ReverseFieldOrder a))   where toJSON = error "ReverseFieldOrder"
instance FromJSON (Key (ReverseFieldOrder a)) where parseJSON = error "ReverseFieldOrder"
instance (PersistEntity a) => PersistEntity (ReverseFieldOrder a) where
    type PersistEntityBackend (ReverseFieldOrder a) = PersistEntityBackend a

    newtype Key (ReverseFieldOrder a) = RFOKey { unRFOKey :: BackendKey SqlBackend } deriving (Show, Read, Eq, Ord, PersistField, PersistFieldSql)
    keyFromValues = fmap RFOKey . fromPersistValue . head
    keyToValues   = (:[]) . toPersistValue . unRFOKey

    entityDef = revFields . entityDef . liftM unRFO
        where
          revFields ed = ed { entityFields = reverse (entityFields ed) }

    toPersistFields = reverse . toPersistFields . unRFO
    newtype EntityField (ReverseFieldOrder a) b = EFRFO {unEFRFO :: EntityField a b}
    persistFieldDef = persistFieldDef . unEFRFO
    fromPersistValues = fmap RFO . fromPersistValues . reverse

    newtype Unique      (ReverseFieldOrder a)   = URFO  {unURFO  :: Unique      a  }
    persistUniqueToFieldNames = reverse . persistUniqueToFieldNames . unURFO
    persistUniqueToValues = reverse . persistUniqueToValues . unURFO
    persistUniqueKeys = map URFO . reverse . persistUniqueKeys . unRFO

    persistIdField = error "ReverseFieldOrder.persistIdField"
    fieldLens = error "ReverseFieldOrder.fieldLens"

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

    transactionSave
    c2 <- count filt
    c2 @== 3

    _ <- insert p
    transactionUndo
    c3 <- count filt
    c3 @== 3

    _ <- insert p
    transactionSave
    _ <- insert p
    _ <- insert p
    transactionUndo
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

#endif

-- Test proper polymorphism
_polymorphic :: (MonadIO m, PersistQuery backend, BaseBackend backend ~ PersistEntityBackend Pet) => ReaderT backend m ()
_polymorphic = do
    ((Entity id' _):_) <- selectList [] [LimitTo 1]
    _ <- selectList [PetOwnerId ==. id'] []
    _ <- insert $ Pet id' "foo" Cat
    return ()

-- Some lens stuff
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . (l (\_ -> Identity b))

type Getting r s t a b = (a -> Constant r b) -> s -> Constant r t

view :: s -> Getting a s t a b -> a
view s l = getConstant (l Constant s)
