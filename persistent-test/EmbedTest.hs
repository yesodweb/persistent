{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module EmbedTest (specs,
#ifndef WITH_MONGODB
embedMigrate
#endif
) where

import Init

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

#if WITH_MONGODB
mkPersist persistSettings [persist|
#else
share [mkPersist sqlSettings,  mkMigrate "embedMigrate"] [persist|
#endif

  DoubleEmbed no-migrate
    name String
    deriving Show Eq Read Ord

  HasEmbed no-migrate
    name String
    embed DoubleEmbed
    deriving Show Eq Read Ord

  HasEmbeds
    name String
    embed HasEmbed
    double_ DoubleEmbed
    deriving Show Eq Read Ord

  HasListEmbed
    name String
    list [HasEmbed]
    deriving Show Eq Read Ord

  HasSetEmbed
    name String
    set_ (S.Set HasEmbed)
    deriving Show Eq Read Ord

  HasMapEmbed
    name String
    map (M.Map T.Text T.Text)
    deriving Show Eq Read Ord
|]
#ifdef WITH_MONGODB
cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter HasEmbed])
  deleteWhere ([] :: [Filter HasEmbeds])
  deleteWhere ([] :: [Filter HasListEmbed])
  deleteWhere ([] :: [Filter HasSetEmbed])
  deleteWhere ([] :: [Filter HasMapEmbed])
db = db' cleanDB
#endif

specs :: Specs
specs = describe "embedded entities" $ do
  it "simple entities" $ db $ do
      let container = HasEmbeds "container"
            (HasEmbed "embed" (DoubleEmbed "1")) (DoubleEmbed "2")
      contK <- insert container
      Just res <- selectFirst [HasEmbedsName ==. "container"] []
      res @== (Entity contK container)

  it "Set" $ db $ do
      let container = HasSetEmbed "set" $ S.fromList [
              (HasEmbed "embed" (DoubleEmbed "1"))
            , (HasEmbed "embed" (DoubleEmbed "2"))
            ]
      contK <- insert container
      Just res <- selectFirst [HasSetEmbedName ==. "set"] []
      res @== (Entity contK container)

  it "List" $ db $ do
      let container = HasListEmbed "list" [
              (HasEmbed "embed" (DoubleEmbed "1"))
            , (HasEmbed "embed" (DoubleEmbed "2"))
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

