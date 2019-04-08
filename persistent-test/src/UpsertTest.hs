{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module UpsertTest where

import Init

import PersistentTestModels

import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Function (on)
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M
import qualified Data.Map as Map
import Test.HUnit hiding (Test)
import Test.Hspec.Expectations ()
import Test.Hspec.QuickCheck(prop)
import UnliftIO (MonadUnliftIO, catch)
import Web.PathPieces (PathPiece (..))
-- import Database.Persist.MongoDB (MongoContext)

import qualified MpsNoPrefixTest
import qualified RawSqlTest
import PersistentTestModels

import Database.Persist

data BackendNullUpdateBehavior
    = AssumeNullIsZero
    | Don'tUpdateNull

specsWith
    :: forall backend m.
    ( MonadIO m, MonadFail m, MonadUnliftIO m
    , PersistStoreWrite backend
    , PersistStoreRead (BaseBackend backend)
    , BaseBackend backend ~ backend
    , HasPersistBackend backend
    , Init.GenerateKey backend
    , PersistUniqueWrite backend
    , PersistQueryWrite backend
    , PersistQueryRead backend
    )
    => RunDb backend m
    -> BackendNullUpdateBehavior
    -> Spec
specsWith runDb handleNull = describe "UpsertTests" $ do
  describe "upsert" $ do
    it "adds a new row with no updates" $ runDb $ do
        Entity _ u <- upsert (Upsert "a" "new" "" 2) [UpsertAttr =. "update"]
        c <- count ([] :: [Filter (UpsertGeneric backend)])
        c @== 1
        upsertAttr u @== "new"
    it "keeps the existing row" $ runDb $ do
        initial <- insertEntity (Upsert "a" "initial" "" 1)
        update' <- upsert (Upsert "a" "update" "" 2) []
        update' @== initial
    it "updates an existing row - assignment" $ runDb $ do
-- #ifdef WITH_MONGODB
--         initial <- insertEntity (Upsert "cow" "initial" "extra" 1)
--         update' <-
--             upsert (Upsert "cow" "wow" "such unused" 2) [UpsertAttr =. "update"]
--         ((==@) `on` entityKey) initial update'
--         upsertAttr (entityVal update') @== "update"
--         upsertExtra (entityVal update') @== "extra"
-- #else
        initial <- insertEntity (Upsert "a" "initial" "extra" 1)
        update' <-
            upsert (Upsert "a" "wow" "such unused" 2) [UpsertAttr =. "update"]
        ((==@) `on` entityKey) initial update'
        upsertAttr (entityVal update') @== "update"
        upsertExtra (entityVal update') @== "extra"
-- #endif
    it "updates existing row - addition " $ runDb $ do
-- #ifdef WITH_MONGODB
--         initial <- insertEntity (Upsert "a1" "initial" "extra" 2)
--         update' <-
--             upsert (Upsert "a1" "wow" "such unused" 2) [UpsertAge +=. 3]
--         ((==@) `on` entityKey) initial update'
--         upsertAge (entityVal update') @== 5
--         upsertExtra (entityVal update') @== "extra"
-- #else
        initial <- insertEntity (Upsert "a" "initial" "extra" 2)
        update' <-
            upsert (Upsert "a" "wow" "such unused" 2) [UpsertAge +=. 3]
        ((==@) `on` entityKey) initial update'
        upsertAge (entityVal update') @== 5
        upsertExtra (entityVal update') @== "extra"
-- #endif

  describe "upsertBy" $ do
    let uniqueEmail = UniqueUpsertBy "a"
        uniqueCity = UniqueUpsertByCity "Boston"
    it "adds a new row with no updates" $ runDb $ do
        Entity _ u <-
            upsertBy
                uniqueEmail
                (UpsertBy "a" "Boston" "new")
                [UpsertByAttr =. "update"]
        c <- count ([] :: [Filter (UpsertByGeneric backend)])
        c @== 1
        upsertByAttr u @== "new"
    it "keeps the existing row" $ runDb $ do
-- #ifdef WITH_MONGODB
--        initial <- insertEntity (UpsertBy "foo" "Chennai" "initial")
--        update' <- upsertBy (UniqueUpsertBy "foo") (UpsertBy "foo" "Chennai" "update") []
--        update' @== initial
-- #else
        initial <- insertEntity (UpsertBy "a" "Boston" "initial")
        update' <- upsertBy uniqueEmail (UpsertBy "a" "Boston" "update") []
        update' @== initial
-- #endif
    it "updates an existing row" $ runDb $ do
-- #ifdef WITH_MONGODB
--         initial <- insertEntity (UpsertBy "ko" "Kumbakonam" "initial")
--         update' <-
--             upsertBy
--                 (UniqueUpsertBy "ko")
--                 (UpsertBy "ko" "Bangalore" "such unused")
--                 [UpsertByAttr =. "update"]
--         ((==@) `on` entityKey) initial update'
--         upsertByAttr (entityVal update') @== "update"
--         upsertByCity (entityVal update') @== "Kumbakonam"
-- #else
        initial <- insertEntity (UpsertBy "a" "Boston" "initial")
        update' <-
            upsertBy
                uniqueEmail
                (UpsertBy "a" "wow" "such unused")
                [UpsertByAttr =. "update"]
        ((==@) `on` entityKey) initial update'
        upsertByAttr (entityVal update') @== "update"
        upsertByCity (entityVal update') @== "Boston"
-- #endif
    it "updates by the appropriate constraint" $ runDb $ do
        initBoston <- insertEntity (UpsertBy "bos" "Boston" "bos init")
        initKrum <- insertEntity (UpsertBy "krum" "Krum" "krum init")
        updBoston <-
            upsertBy
                (UniqueUpsertBy "bos")
                (UpsertBy "bos" "Krum" "unused")
                [UpsertByAttr =. "bos update"]
        updKrum <-
            upsertBy
                (UniqueUpsertByCity "Krum")
                (UpsertBy "bos" "Krum" "unused")
                [UpsertByAttr =. "krum update"]
        ((==@) `on` entityKey) initBoston updBoston
        ((==@) `on` entityKey) initKrum updKrum
        entityVal updBoston @== UpsertBy "bos" "Boston" "bos update"
        entityVal updKrum @== UpsertBy "krum" "Krum" "krum update"

  it "maybe update" $ runDb $ do
      let noAge = PersonMaybeAge "Michael" Nothing
      keyNoAge <- insert noAge
      noAge2 <- updateGet keyNoAge [PersonMaybeAgeAge +=. Just 2]
      -- the correct answer depends on the backend. MongoDB assumes
      -- a 'Nothing' value is 0, and does @0 + 2@ for @Just 2@. In a SQL
      -- database, @NULL@ annihilates, so @NULL + 2 = NULL@.
      personMaybeAgeAge noAge2 @== case handleNull of
          AssumeNullIsZero ->
              Just 2
          Don'tUpdateNull ->
              Nothing

