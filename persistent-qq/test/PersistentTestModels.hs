{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PersistentTestModels where

import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Data.Text (Text)

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NEL
import Database.Persist.Sql
import Database.Persist.TH
import PersistTestPetCollarType
import PersistTestPetType

share
    [ mkPersist sqlSettings
    , mkMigrate "testMigrate"
    ] [persistUpperCase|

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
    deriving Show Eq
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
    attr Text
    extra Text
    age Int
    UniqueUpsert email
    deriving Eq Show

  UpsertBy
    email Text
    city Text
    attr Text
    UniqueUpsertBy email
    UniqueUpsertByCity city
    deriving Eq Show

  Strict
    !yes Int
    ~no Int
    def Int

|]

share [ mkPersist sqlSettings { mpsPrefixFields = False }
      , mkMigrate "noPrefixMigrate"
      ] [persistLowerCase|
NoPrefix1
    someFieldName Int
NoPrefix2
    someOtherFieldName Int
    unprefixedRef NoPrefix1Id
+NoPrefixSum
    unprefixedLeft Int
    unprefixedRight String
    deriving Show Eq
|]

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

    entityDef = revFields . entityDef . unRfoProxy
      where
        revFields = overEntityFields reverse
        unRfoProxy :: proxy (ReverseFieldOrder a) -> Proxy a
        unRfoProxy _ = Proxy

    toPersistFields = reverse . toPersistFields . unRFO
    newtype EntityField (ReverseFieldOrder a) b = EFRFO {unEFRFO :: EntityField a b}
    persistFieldDef = persistFieldDef . unEFRFO
    fromPersistValues = fmap RFO . fromPersistValues . reverse

    newtype Unique      (ReverseFieldOrder a)   = URFO  {unURFO  :: Unique      a  }
    persistUniqueToFieldNames = NEL.reverse . persistUniqueToFieldNames . unURFO
    persistUniqueToValues = reverse . persistUniqueToValues . unURFO
    persistUniqueKeys = map URFO . reverse . persistUniqueKeys . unRFO

    persistIdField = error "ReverseFieldOrder.persistIdField"
    fieldLens x = error "ReverseFieldOrder.fieldLens"

cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = do
  deleteWhere ([] :: [Filter Person])
  deleteWhere ([] :: [Filter Person1])
  deleteWhere ([] :: [Filter Pet])
  deleteWhere ([] :: [Filter MaybeOwnedPet])
  deleteWhere ([] :: [Filter NeedsPet])
  deleteWhere ([] :: [Filter OutdoorPet])
  deleteWhere ([] :: [Filter UserPT])
  deleteWhere ([] :: [Filter EmailPT])
