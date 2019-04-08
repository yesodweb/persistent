{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module RenameTest where

#ifndef WITH_NOSQL
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
#endif
import Data.Time (getCurrentTime, Day, UTCTime(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import Init

-- persistent used to not allow types with an "Id" suffix
-- this verifies that the issue is fixed
type TextId = Text

-- Test lower case names
#if WITH_NOSQL
mkPersist persistSettings { mpsGeneric = True } [persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "migration"] [persistLowerCase|
#endif
-- This just tests that a field can be named "key"
KeyTable
    key Text
    deriving Eq Show

IdTable
    Id   Day default=CURRENT_DATE
    name Text
    -- This was added to test the ability to break a cycle
    -- getting rid of the Maybe should be a compilation failure
    keyTableEmbed IdTable Maybe
    deriving Eq Show

LowerCaseTable
    Id            sql=my_id
    fullName Text
    ExtraBlock
        foo bar
        baz
        bin
    ExtraBlock2
        something

RefTable
    someVal Int sql=something_else
    lct LowerCaseTableId
    text TextId
    UniqueRefTable someVal

-- Test a reference to a non-int Id
ForeignIdTable
    idId IdTableId
|]

cleanDB
    :: forall backend.
    ( BaseBackend backend ~ backend
    , PersistQueryWrite backend
    )
    => ReaderT backend IO ()
cleanDB = do
  deleteWhere ([] :: [Filter (IdTableGeneric backend)])
  deleteWhere ([] :: [Filter (LowerCaseTableGeneric backend)])
  deleteWhere ([] :: [Filter (RefTableGeneric backend)])

#if WITH_NOSQL
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = specsWith db

specsWith
    ::
    ( PersistStoreWrite backend, PersistQueryRead backend
    , backend ~ BaseBackend backend
    , MonadIO m, MonadFail m
    , Eq (BackendKey backend)
    )
    => RunDb backend m
    -> Spec
specsWith runDb = describe "rename specs" $ do
    it "user specified id, insertKey, no default=" $ runDb $ do
      let rec2 = IdTable "Foo2" Nothing
      let rec1 = IdTable "Foo1" $ Just rec2
      let rec  = IdTable "Foo" $ Just rec1
      now <- liftIO getCurrentTime
      let key = IdTableKey $ utctDay now
      insertKey key rec
      Just rec' <- get key
      rec' @== rec
      (Entity key' _):_ <- selectList ([] :: [Filter (IdTableGeneric backend)]) []
      key' @== key

#ifndef WITH_MYSQL
#  ifndef WITH_NOSQL
    -- this uses default=
    it "user specified id, default=" $ runDb $ do
      liftIO $ pendingWith "This test should only run against SQLite and Postgresql."
      let rec = IdTable "Foo" Nothing
      k <- insert rec
      Just rec' <- get k
      rec' @== rec
#  endif
#endif

    it "extra blocks" $
        entityExtra (entityDef (Nothing :: Maybe LowerCaseTable)) @?=
            Map.fromList
                [ ("ExtraBlock", map T.words ["foo bar", "baz", "bin"])
                , ("ExtraBlock2", map T.words ["something"])
                ]
