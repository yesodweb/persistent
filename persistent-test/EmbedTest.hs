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
import Test.HUnit (Assertion)
import Test.Hspec (shouldThrow)
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Database.Persist.MongoDB

data TestException = TestException
    deriving (Show, Typeable, Eq)
instance Exception TestException

#if WITH_MONGODB
mkPersist persistSettings [persist|
#else
share [mkPersist sqlSettings,  mkMigrate "embedMigrate"] [persistLowerCase|
#endif

  OnlyName
    name String
    deriving Show Eq Read Ord

  HasEmbed
    name String
    embed OnlyName
    deriving Show Eq Read Ord

  HasEmbeds
    name String
    embed OnlyName
    double HasEmbed
    deriving Show Eq Read Ord

  HasListEmbed
    name String
    list [HasEmbed]
    deriving Show Eq Read Ord

  HasSetEmbed
    name String
    set (S.Set HasEmbed)
    deriving Show Eq Read Ord

  HasMapEmbed
    name String
    map (M.Map T.Text T.Text)
    deriving Show Eq Read Ord


  User
    ident T.Text
    password T.Text Maybe
    profile Profile
    deriving Show Eq Read Ord

  Profile
    firstName T.Text
    lastName T.Text
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

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "embedded entities" $ do
  it "simple entities" $ db $ do
      let container = HasEmbeds "container" (OnlyName "2")
            (HasEmbed "embed" (OnlyName "1"))
      contK <- insert container
      Just res <- selectFirst [HasEmbedsName ==. "container"] []
      res @== (Entity contK container)

  it "query for equality of embeded entity" $ db $ do
      let container = HasEmbed "container" (OnlyName "2")
      contK <- insert container
      Just res <- selectFirst [HasEmbedEmbed ==. (OnlyName "2")] []
      res @== (Entity contK container)

  it "Set" $ db $ do
      let container = HasSetEmbed "set" $ S.fromList [
              (HasEmbed "embed" (OnlyName "1"))
            , (HasEmbed "embed" (OnlyName "2"))
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set"] []
      res @== (Entity contK container)

  it "exception" $ flip shouldThrow (== TestException) $ db $ do
      let container = HasSetEmbed "set" $ S.fromList [
              (HasEmbed "embed" (OnlyName "1"))
            , (HasEmbed "embed" (OnlyName "2"))
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. throw TestException] []
      res @== (Entity contK container)

  it "List" $ db $ do
      let container = HasListEmbed "list" [
              (HasEmbed "embed" (OnlyName "1"))
            , (HasEmbed "embed" (OnlyName "2"))
            ]
      contK <- insert container
      Just res <- selectFirst [HasListEmbedName ==. "list"] []
      res @== (Entity contK container)

  it "Map" $ db $ do
      let container = HasMapEmbed "map" $ M.fromList [
              ("k1","v1")
            , ("k2","v2")
            ]
      contK <- insert container
      Just res <- selectFirst [HasMapEmbedName ==. "map"] []
      res @== (Entity contK container)

#ifdef WITH_MONGODB
  it "mongo filters" $ db $ do
      let usr = User "foo" (Just "pswd") prof
          prof = Profile "fstN" "lstN" (Just con)
          con = Contact 123456 "foo@bar.com"
      uId <- insert usr
      Just r1 <- selectFirst [UserProfile ->. ProfileFirstName `nestEq` "fstN"] []
      r1 @== (Entity uId usr)
      Just r2 <- selectFirst [UserProfile ~>. ProfileContact ?->. ContactEmail `nestEq` "foo@bar.com", UserIdent ==. "foo"] []
      r2 @== (Entity uId usr)

      let container = HasListEmbed "list" [
              (HasEmbed "embed" (OnlyName "1"))
            , (HasEmbed "embed" (OnlyName "2"))
            ]
      contK <- insert container
      Just res <- selectFirst [HasListEmbedList `multiEq` HasEmbed "embed" (OnlyName "1")] []
      res @== (Entity contK container)
      return ()
#endif
