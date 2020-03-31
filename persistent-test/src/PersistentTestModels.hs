{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-} -- FIXME
module PersistentTestModels where

import Data.Aeson

import Test.QuickCheck
import Database.Persist.Sql
import Database.Persist.TH
import Init
import PersistTestPetType
import PersistTestPetCollarType
import Data.Text (append)

-- just need to ensure this compiles
import PersistentTestModelsImports()

share [mkPersist persistSettings { mpsGeneric = True },  mkMigrate "testMigrate", mkDeleteCascade persistSettings, mkSave "_ignoredSave"] [persistUpperCase|

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

  -- | This is a doc comment for a relationship.
  -- | You need to put the pipe character for each line of documentation.
  -- Lines without a pipe are omitted.
  -- | But you can resume the doc comments afterwards.
  Relationship
      -- | Fields should be documentable.
      name String
      parent RelationshipId Maybe

  MutA
    mutB    MutBId

  MutB
    mutA    MutAId
|]

deriving instance Show (BackendKey backend) => Show (PetGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (PetGeneric backend)

deriving instance Show (BackendKey backend) => Show (RelationshipGeneric backend)
deriving instance Eq (BackendKey backend) => Eq (RelationshipGeneric backend)

share [mkPersist persistSettings { 
          mpsPrefixFields = False
        , mpsFieldLabelModifier = \_ _ -> "" -- this field is ignored when mpsPrefixFields == False
        , mpsConstraintLabelModifier = \_ _ -> "" -- this field is ignored when mpsPrefixFields == False
        , mpsGeneric = True 
        }
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

share [mkPersist sqlSettings] [persistLowerCase|
JsonEncoding json
    name Text
    age  Int
    Primary name
    deriving Show Eq

JsonEncoding2 json
    name Text
    age Int
    blood Text
    Primary name blood
    deriving Show Eq
|]

instance Arbitrary JsonEncoding where
    arbitrary = JsonEncoding <$> arbitrary <*> arbitrary

instance Arbitrary JsonEncoding2 where
    arbitrary = JsonEncoding2 <$> arbitrary <*> arbitrary <*> arbitrary

deriving instance Show (BackendKey backend) => Show (NoPrefix1Generic backend)
deriving instance Eq (BackendKey backend) => Eq (NoPrefix1Generic backend)

deriving instance Show (BackendKey backend) => Show (NoPrefix2Generic backend)
deriving instance Eq (BackendKey backend) => Eq (NoPrefix2Generic backend)

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
        , mpsGeneric = True
        }
      , mkMigrate "customPrefixMigrate"
      ] [persistLowerCase|
CustomPrefix1
    customFieldName Int
CustomPrefix2
    otherCustomFieldName Int
    customPrefixedRef CustomPrefix1Id
+CustomPrefixSum
    customPrefixedLeft Int
    customPrefixedRight String
    deriving Show Eq
|]

deriving instance Show (BackendKey backend) => Show (CustomPrefix1Generic backend)
deriving instance Eq (BackendKey backend) => Eq (CustomPrefix1Generic backend)

deriving instance Show (BackendKey backend) => Show (CustomPrefix2Generic backend)
deriving instance Eq (BackendKey backend) => Eq (CustomPrefix2Generic backend)

share [mkPersist persistSettings { mpsPrefixFields = False, mpsGeneric = False }
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
