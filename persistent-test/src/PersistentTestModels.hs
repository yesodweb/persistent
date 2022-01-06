{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
module PersistentTestModels where

import Data.Aeson

import qualified Data.List.NonEmpty as NEL
import Data.Proxy
import Test.QuickCheck
import Database.Persist.Sql
import Database.Persist.TH
import Init
import PersistTestPetType
import PersistTestPetCollarType
import Data.Text (append)

-- just need to ensure this compiles
import PersistentTestModelsImports()

share [mkPersist persistSettings,  mkMigrate "testMigrate"] [persistUpperCase|

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
-- Dedented comment
  -- Header-level comment
    -- Indented comment
    type PetType
    deriving Show Eq

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

  DudeWeirdColumns
    name Text
    foo  Int     Maybe MigrationOnly
    bar  Double  SafeToRemove
    UniqueName name
    deriving Eq Show

  -- | This is a doc comment for a relationship.
  -- | You need to put the pipe character for each line of documentation.
  -- Lines without a pipe are omitted.
  -- | But you can resume the doc comments afterwards.
  Relationship
      -- | Fields should be documentable.
      name String
      parent RelationshipId Maybe
      deriving Show Eq

  MutA
    mutB    MutBId

  MutB
    mutA    MutAId

|]

share [mkPersist persistSettings {
          mpsPrefixFields = False
        , mpsFieldLabelModifier = \_ _ -> "" -- this field is ignored when mpsPrefixFields == False
        , mpsConstraintLabelModifier = \_ _ -> "" -- this field is ignored when mpsPrefixFields == False
        }
      , mkMigrate "noPrefixMigrate"
      ] [persistLowerCase|
NoPrefix1
    someFieldName Int
    deriving Eq Show
NoPrefix2
    someOtherFieldName Int
    unprefixedRef NoPrefix1Id
    deriving Eq Show
+NoPrefixSum
    unprefixedLeft Int
    unprefixedRight String
    deriving Show Eq

|]

share [mkPersist persistSettings {
          mpsFieldLabelModifier = \entity field -> case entity of
            "CustomPrefix1" -> append "_cp1" field
            "CustomPrefix2" -> append "_cp2" field
            _ -> error "should not be called"
        , mpsConstraintLabelModifier = \entity field -> case entity of
            "CustomPrefix1" -> append "CP1" field
            "CustomPrefix2" -> append "CP2" field
            "CustomPrefixSum" -> append "CP" field
            _ -> error "should not be called"
        }
      , mkMigrate "customPrefixMigrate"
      ] [persistLowerCase|
CustomPrefix1
    customFieldName Int
    deriving Show Eq
CustomPrefix2
    otherCustomFieldName Int
    customPrefixedRef CustomPrefix1Id
+CustomPrefixSum
    customPrefixedLeft Int
    customPrefixedRight String
    deriving Show Eq
|]

share [mkPersist persistSettings { mpsPrefixFields = False }
      , mkMigrate "treeMigrate"
      ] [persistLowerCase|

Tree sql=trees
  name String
  parent String Maybe
  Primary name
  Foreign Tree fkparent parent
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
        unRfoProxy :: proxy (ReverseFieldOrder a) -> Proxy a
        unRfoProxy _ = Proxy
        revFields = overEntityFields reverse

    toPersistFields = reverse . toPersistFields . unRFO
    newtype EntityField (ReverseFieldOrder a) b = EFRFO {unEFRFO :: EntityField a b}
    persistFieldDef = persistFieldDef . unEFRFO
    fromPersistValues = fmap RFO . fromPersistValues . reverse

    newtype Unique      (ReverseFieldOrder a)   = URFO  {unURFO  :: Unique      a  }
    persistUniqueToFieldNames = NEL.reverse . persistUniqueToFieldNames . unURFO
    persistUniqueToValues = reverse . persistUniqueToValues . unURFO
    persistUniqueKeys = fmap URFO . reverse . persistUniqueKeys . unRFO

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
  deleteWhere ([] :: [Filter Upsert])
  deleteWhere ([] :: [Filter UpsertBy])
