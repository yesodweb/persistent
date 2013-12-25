{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module EmbedTest (specs,
#ifndef WITH_MONGODB
embedMigrate
#endif
) where

import Init
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
#if WITH_MONGODB
import Database.Persist.MongoDB
import Database.MongoDB (Value(String))
import EntityEmbedTest
import System.Process (readProcess)
#endif
import System.Environment (getEnvironment)
import Control.Monad.IO.Class
import Control.Monad (unless)

data TestException = TestException
    deriving (Show, Typeable, Eq)
instance Exception TestException

#if WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
  HasObjectId
    oid  Objectid
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

#else
share [mkPersist sqlSettings,  mkMigrate "embedMigrate"] [persistUpperCase|
#endif

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

  HasMapEmbed
    name Text
    map (M.Map T.Text T.Text)
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

|]
#ifdef WITH_MONGODB
cleanDB :: (PersistQuery m, PersistEntityBackend HasMapEmbed ~ PersistMonadBackend m) => m ()
cleanDB = do
  deleteWhere ([] :: [Filter HasEmbed])
  deleteWhere ([] :: [Filter HasEmbeds])
  deleteWhere ([] :: [Filter HasListEmbed])
  deleteWhere ([] :: [Filter HasSetEmbed])
  deleteWhere ([] :: [Filter User])
  deleteWhere ([] :: [Filter HasMapEmbed])
  deleteWhere ([] :: [Filter ListEmbed])
  deleteWhere ([] :: [Filter ARecord])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

unlessM :: MonadIO m => IO Bool -> m () -> m ()
unlessM predicate body = do
    b <- liftIO predicate
    unless b body

isTravis :: IO Bool
isTravis = do
  env <- liftIO getEnvironment
  return $ case lookup "TRAVIS" env of
    Just "true" -> True
    _ -> False

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

  it "exception" $ flip shouldThrow (== TestException) $ db $ do
      let container = HasSetEmbed "set" $ S.fromList
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. throw TestException] []
      res @== Entity contK container

  it "List" $ db $ do
      let container = HasListEmbed "list"
            [ HasEmbed "embed" (OnlyName "1")
            , HasEmbed "embed" (OnlyName "2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list"] []
      res @== Entity contK container

  it "Map" $ db $ do
      let container = HasMapEmbed "map" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasMapEmbedName ==. "map"] []
      res @== Entity contK container

#ifdef WITH_MONGODB
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
    oid <- liftIO $ genObjectid
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
        Just meq <- selectFirst [HasListEmbedList `multiEq` HasEmbed "embed" (OnlyName "1")] []
        meq @== (Entity contK container)

        Just neq1 <- selectFirst [HasListEmbedList ->. HasEmbedName `nestEq` "embed"] []
        neq1 @== (Entity contK container)

        Just neq2 <- selectFirst [HasListEmbedList ~>. HasEmbedEmbed &->. OnlyNameName `nestEq` "1"] []
        neq2 @== (Entity contK container)

        Just nbq1 <- selectFirst [HasListEmbedList ->. HasEmbedName `nestBsonEq` String "embed"] []
        nbq1 @== (Entity contK container)

        Just nbq2 <- selectFirst [HasListEmbedList ~>. HasEmbedEmbed &->. OnlyNameName `nestBsonEq` String "1"] []
        nbq2 @== (Entity contK container)


    it "regexp match" $ db $ do
        let container = HasListEmbed "list" [
                (HasEmbed "embed" (OnlyName "abcd"))
              , (HasEmbed "embed" (OnlyName "efgh"))
              ]
        contK <- insert container
        let mkReg t = (t, "ims")
        Just res <- selectFirst [HasListEmbedName =~. mkReg "ist"] []
        res @== (Entity contK container)


  it "re-orders json inserted from another source" $ db $ do
    _ <- liftIO $ readProcess "mongoimport" ["-d", "test", "-c", "ListEmbed"] "{ \"nested\": [{ \"one\": 1, \"two\": 2 }, { \"two\": 2, \"one\": 1}], \"two\": 2, \"one\": 1, \"_id\" : { \"$oid\" : \"50184f5a92d7ae0000001e89\" } }"
    (Entity _ list):[] <- selectList [] []
    list @== ListEmbed [InList 1 2, InList 1 2] 1 2
    return ()
#endif
