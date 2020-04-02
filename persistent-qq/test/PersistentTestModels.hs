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
{-# LANGUAGE UndecidableInstances #-} -- FIXME
{-# LANGUAGE DerivingStrategies #-}
module PersistentTestModels where

import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)

import Database.Persist.Sql
import Database.Persist.TH
import PersistTestPetType
import PersistTestPetCollarType

share
    [ mkPersist sqlSettings { mpsGeneric = True }
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

deriving instance Show (BackendKey backend) => Show (PetGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (PetGeneric backend)

share [ mkPersist sqlSettings { mpsPrefixFields = False, mpsGeneric = True }
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

deriving instance Show (BackendKey backend) => Show (NoPrefix1Generic backend)
deriving instance Eq (BackendKey backend) => Eq (NoPrefix1Generic backend)

deriving instance Show (BackendKey backend) => Show (NoPrefix2Generic backend)
deriving instance Eq (BackendKey backend) => Eq (NoPrefix2Generic backend)

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

cleanDB
    :: (MonadIO m, PersistQuery backend, PersistStoreWrite (BaseBackend backend))
    => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (PersonGeneric backend)])
  deleteWhere ([] :: [Filter (Person1Generic backend)])
  deleteWhere ([] :: [Filter (PetGeneric backend)])
  deleteWhere ([] :: [Filter (MaybeOwnedPetGeneric backend)])
  deleteWhere ([] :: [Filter (NeedsPetGeneric backend)])
  deleteWhere ([] :: [Filter (OutdoorPetGeneric backend)])
  deleteWhere ([] :: [Filter (UserPTGeneric backend)])
  deleteWhere ([] :: [Filter (EmailPTGeneric backend)])
