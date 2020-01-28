{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-orphans -O0 #-}
module EmbedTestMongo (specs) where

import MongoInit

import Control.Exception (Exception, throw)
import Data.List.NonEmpty hiding (insert, length)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database.MongoDB (genObjectId)
import Database.MongoDB (Value(String))
import System.Process (readProcess)

import EntityEmbedTestMongo
import Database.Persist.MongoDB

data TestException = TestException
    deriving (Show, Typeable, Eq)
instance Exception TestException

instance PersistFieldSql a => PersistFieldSql (NonEmpty a) where
    sqlType _ = SqlString

instance PersistField a => PersistField (NonEmpty a) where
    toPersistValue = toPersistValue . toList
    fromPersistValue pv = do
        list <- fromPersistValue pv
        case list of
            [] -> Left "PersistField: NonEmpty found unexpected Empty List"
            (l:ls) -> Right (l:|ls)


mkPersist persistSettings [persistUpperCase|
  HasObjectId
    oid  ObjectId
    name Text
    deriving Show Eq Read Ord

  HasArrayWithObjectIds
    name Text
    arrayWithObjectIds [HasObjectId]
    deriving Show Eq Read Ord

  HasArrayWithEntities
    hasEntity (Entity ARecord)
    arrayWithEntities [AnEntity]
    deriving Show Eq Read Ord

  OnlyName
    name Text
    deriving Show Eq Read Ord

  HasEmbed
    name Text
    embed OnlyName
    deriving Show Eq Read Ord

  HasEmbeds
    name Text
    embed OnlyName
    double HasEmbed
    deriving Show Eq Read Ord

  HasListEmbed
    name Text
    list [HasEmbed]
    deriving Show Eq Read Ord

  HasSetEmbed
    name Text
    set (S.Set HasEmbed)
    deriving Show Eq Read Ord

  HasMap
    name Text
    map (M.Map T.Text T.Text)
    deriving Show Eq Read Ord

  HasList
    list [HasListId]
    deriving Show Eq Read Ord

  EmbedsHasMap
    name Text Maybe
    embed HasMap
    deriving Show Eq Read Ord

  InList
    one Int
    two Int
    deriving Show Eq

  ListEmbed
    nested [InList]
    one Int
    two Int
    deriving Show Eq

  User
    ident Text
    password Text Maybe
    profile Profile
    deriving Show Eq Read Ord

  Profile
    firstName Text
    lastName Text
    contact Contact Maybe
    deriving Show Eq Read Ord

  Contact
    phone Int
    email T.Text
    deriving Show Eq Read Ord

  Account
    userIds       (NonEmpty (Key User))
    name          Text Maybe
    customDomains [Text]             -- we may want to allow multiple cust domains.  use [] instead of maybe

    deriving Show Eq Read Ord

  HasNestedList
    list [IntList]
    deriving Show Eq

  IntList
    ints [Int]
    deriving Show Eq

  -- We would like to be able to use OnlyNameId
  -- But (Key OnlyName) works
  MapIdValue
    map (M.Map T.Text (Key OnlyName))
    deriving Show Eq Read Ord


  -- Self refrences are only allowed as a nullable type:
  -- a Maybe or a List
  SelfList
    reference [SelfList]

  SelfMaybe
    reference SelfMaybe Maybe

  -- This failes
  -- SelfDirect
  --  reference SelfDirect
|]

cleanDB :: (PersistQuery backend, PersistEntityBackend HasMap ~ backend, MonadIO m) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter HasEmbed])
  deleteWhere ([] :: [Filter HasEmbeds])
  deleteWhere ([] :: [Filter HasListEmbed])
  deleteWhere ([] :: [Filter HasSetEmbed])
  deleteWhere ([] :: [Filter User])
  deleteWhere ([] :: [Filter HasMap])
  deleteWhere ([] :: [Filter HasList])
  deleteWhere ([] :: [Filter EmbedsHasMap])
  deleteWhere ([] :: [Filter ListEmbed])
  deleteWhere ([] :: [Filter ARecord])
  deleteWhere ([] :: [Filter Account])
  deleteWhere ([] :: [Filter HasNestedList])

db :: Action IO () -> Assertion
db = db' cleanDB

unlessM :: MonadIO m => IO Bool -> m () -> m ()
unlessM predicate body = do
    b <- liftIO predicate
    unless b body

specs :: Spec
specs = describe "embedded entities" $ do

  it "simple entities" $ db $ do
      let container = HasEmbeds "container" (OnlyName "2")
            (HasEmbed "embed" (OnlyName "1"))
      contK <- insert container
      Just res <- selectFirst [HasEmbedsName ==. "container"] []
      res @== Entity contK container

  it "query for equality of embeded entity" $ db $ do
      let container = HasEmbed "container" (OnlyName "2")
      contK <- insert container
      Just res <- selectFirst [HasEmbedEmbed ==. OnlyName "2"] []
      res @== Entity contK container

  it "Set" $ db $ do
      let container = HasSetEmbed "set" $ S.fromList
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set"] []
      res @== Entity contK container

  it "Set empty" $ db $ do
      let container = HasSetEmbed "set empty" $ S.fromList []
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set empty"] []
      res @== Entity contK container

  it "exception" $ flip shouldThrow (== TestException) $ db $ do
      let container = HasSetEmbed "set" $ S.fromList
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. throw TestException] []
      res @== Entity contK container

  it "ListEmbed" $ db $ do
      let container = HasListEmbed "list"
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list"] []
      res @== Entity contK container

  it "ListEmbed empty" $ db $ do
      let container = HasListEmbed "list empty" []
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list empty"] []
      res @== Entity contK container

  it "List empty" $ db $ do
      let container = HasList []
      contK <- insert container
      Just res <- selectFirst [] []
      res @== Entity contK container

  it "NonEmpty List wrapper" $ db $ do
      let con = Contact 123456 "foo@bar.com"
      let prof = Profile "fstN" "lstN" (Just con)
      uid <- insert $ User "foo" (Just "pswd") prof
      let container = Account (uid:|[]) (Just "Account") []
      contK <- insert container
      Just res <- selectFirst [AccountUserIds ==. (uid:|[])] []
      res @== Entity contK container

  it "Map" $ db $ do
      let container = HasMap "2 items" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasMapName ==. "2 items"] []
      res @== Entity contK container

  it "Map empty" $ db $ do
      let container = HasMap "empty" $ M.fromList []
      contK <- insert container
      Just res <- selectFirst [HasMapName ==. "empty"] []
      res @== Entity contK container

  it "Embeds a Map" $ db $ do
      let container = EmbedsHasMap (Just "non-empty map") $ HasMap "2 items" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [EmbedsHasMapName ==. Just "non-empty map"] []
      res @== Entity contK container

  it "Embeds a Map empty" $ db $ do
      let container = EmbedsHasMap (Just "empty map") $ HasMap "empty" $ M.fromList []
      contK <- insert container
      Just res <- selectFirst [EmbedsHasMapName ==. Just "empty map"] []
      res @== Entity contK container

  it "Embeds a Map with ids as values" $ db $ do
      onId <- insert $ OnlyName "nombre"
      onId2 <- insert $ OnlyName "nombre2"
      let midValue = MapIdValue $ M.fromList [("foo", onId),("bar",onId2)]
      mK <- insert midValue
      Just mv <- get mK
      mv @== midValue

  it "List" $ db $ do
      k1 <- insert $ HasList []
      k2 <- insert $ HasList [k1]
      let container = HasList [k1, k2]
      contK <- insert container
      Just res <- selectFirst [HasListList `anyEq` k2] []
      res @== Entity contK container

  it "can embed an Entity" $ db $ do
    let foo = ARecord "foo"
        bar = ARecord "bar"
    _ <- insertMany [foo, bar]
    arecords <- selectList ([ARecordName ==. "foo"] ||. [ARecordName ==. "bar"]) []
    length arecords @== 2

    kfoo <- insert foo
    let hasEnts = HasArrayWithEntities (Entity kfoo foo) arecords
    kEnts <- insert hasEnts
    Just retrievedHasEnts <- get kEnts
    retrievedHasEnts @== hasEnts

  it "can embed objects with ObjectIds" $ db $ do
    oid <- liftIO $ genObjectId
    let hoid   = HasObjectId oid "oid"
        hasArr = HasArrayWithObjectIds "array" [hoid]

    k <- insert hasArr
    Just v <- get k
    v @== hasArr

  describe "mongoDB filters" $ do
    it "mongo single nesting filters" $ db $ do
        let usr = User "foo" (Just "pswd") prof
            prof = Profile "fstN" "lstN" (Just con)
            con = Contact 123456 "foo@bar.com"
        uId <- insert usr
        Just r1 <- selectFirst [UserProfile &->. ProfileFirstName `nestEq` "fstN"] []
        r1 @== (Entity uId usr)
        Just r2 <- selectFirst [UserProfile &~>. ProfileContact ?&->. ContactEmail `nestEq` "foo@bar.com", UserIdent ==. "foo"] []
        r2 @== (Entity uId usr)

    it "mongo embedded array filters" $ db $ do
        let container = HasListEmbed "list" [
                (HasEmbed "embed" (OnlyName "1"))
              , (HasEmbed "embed" (OnlyName "2"))
              ]
        contK <- insert container
        let contEnt = Entity contK container
        Just meq <- selectFirst [HasListEmbedList `anyEq` HasEmbed "embed" (OnlyName "1")] []
        meq @== contEnt

        Just neq1 <- selectFirst [HasListEmbedList ->. HasEmbedName `nestEq` "embed"] []
        neq1 @== contEnt

        Just nne1 <- selectFirst [HasListEmbedList ->. HasEmbedName `nestNe` "notEmbed"] []
        nne1 @== contEnt

        Just neq2 <- selectFirst [HasListEmbedList ~>. HasEmbedEmbed &->. OnlyNameName `nestEq` "1"] []
        neq2 @== contEnt

        Just nbq1 <- selectFirst [HasListEmbedList ->. HasEmbedName `nestBsonEq` String "embed"] []
        nbq1 @== contEnt

        Just nbq2 <- selectFirst [HasListEmbedList ~>. HasEmbedEmbed &->. OnlyNameName `nestBsonEq` String "1"] []
        nbq2 @== contEnt

    it "regexp match" $ db $ do
        let container = HasListEmbed "list" [
                (HasEmbed "embed" (OnlyName "abcd"))
              , (HasEmbed "embed" (OnlyName "efgh"))
              ]
        contK <- insert container
        let mkReg t = (t, "ims")
        Just res <- selectFirst [HasListEmbedName =~. mkReg "ist"] []
        res @== (Entity contK container)

    it "nested anyEq" $ db $ do
        let top = HasNestedList [IntList [1,2]]
        k <- insert top
        Nothing  <- selectFirst [HasNestedListList ->. IntListInts `nestEq` ([]::[Int])] []
        Nothing  <- selectFirst [HasNestedListList ->. IntListInts `nestAnyEq` 3] []
        Just res <- selectFirst [HasNestedListList ->. IntListInts `nestAnyEq` 2] []
        res @== (Entity k top)

  describe "mongoDB updates" $ do
    it "mongo single nesting updates" $ db $ do
        let usr = User "foo" (Just "pswd") prof
            prof = Profile "fstN" "lstN" (Just con)
            con = Contact 123456 "foo@bar.com"
        uid <- insert usr
        let newName = "fstN2"
        usr1 <- updateGet uid [UserProfile &->. ProfileFirstName `nestSet` newName]
        (profileFirstName $ userProfile usr1) @== newName

        let newEmail = "foo@example.com"
        let newIdent = "bar"
        usr2 <- updateGet uid [UserProfile &~>. ProfileContact ?&->. ContactEmail `nestSet` newEmail, UserIdent =. newIdent]
        (userIdent usr2) @== newIdent
        (fmap contactEmail . profileContact . userProfile $ usr2) @== Just newEmail


    it "mongo embedded array updates" $ db $ do
        let container = HasListEmbed "list" [
                (HasEmbed "embed" (OnlyName "1"))
              , (HasEmbed "embed" (OnlyName "2"))
              ]
        contk <- insert container
        let _contEnt = Entity contk container

        pushed <- updateGet contk [HasListEmbedList `push` HasEmbed "embed" (OnlyName "3")]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList pushed) @== ["1","2","3"]

        -- same, don't add anything
        addedToSet <- updateGet contk [HasListEmbedList `addToSet` HasEmbed "embed" (OnlyName "3")]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList addedToSet) @== ["1","2","3"]
        pulled <- updateGet contk [HasListEmbedList `pull` HasEmbed "embed" (OnlyName "3")]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList pulled) @== ["1","2"]

        -- now it is new
        addedToSet2 <- updateGet contk [HasListEmbedList `addToSet` HasEmbed "embed" (OnlyName "3")]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList addedToSet2) @== ["1","2","3"]

        allPulled <- updateGet contk [eachOp pull HasListEmbedList
          [ HasEmbed "embed" (OnlyName "3")
          , HasEmbed "embed" (OnlyName "2")
          ] ]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList allPulled) @== ["1"]
        allPushed <- updateGet contk [eachOp push HasListEmbedList
          [ HasEmbed "embed" (OnlyName "4")
          , HasEmbed "embed" (OnlyName "5")
          ] ]
        (Prelude.map (onlyNameName . hasEmbedEmbed) $ hasListEmbedList allPushed) @== ["1","4","5"]


  it "re-orders json inserted from another source" $ db $ do
    let cname = T.unpack $ collectionName (error "ListEmbed" :: ListEmbed)
    liftIO $ putStrLn =<< readProcess "mongoimport" ["-d", T.unpack dbName, "-c", cname] "{ \"nested\": [{ \"one\": 1, \"two\": 2 }, { \"two\": 2, \"one\": 1}], \"two\": 2, \"one\": 1, \"_id\" : { \"$oid\" : \"50184f5a92d7ae0000001e89\" } }"

    lists <- selectList [] []
    fmap entityVal lists @== [ListEmbed [InList 1 2, InList 1 2] 1 2]
