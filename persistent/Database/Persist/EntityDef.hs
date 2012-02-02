module Database.Persist.EntityDef
    ( -- * Helper types
      HaskellName (..)
    , DBName (..)
    , Attr
      -- * Defs
    , EntityDef (..)
    , FieldDef (..)
    , FieldType (..)
    , isEmbedded
    , UniqueDef (..)
    , ExtraLine
    ) where

import Data.Text (Text)
import Data.Map (Map)

data EntityDef = EntityDef
    { entityHaskell :: HaskellName
    , entityDB      :: DBName
    , entityID      :: DBName
    , entityAttrs   :: [Attr]
    , entityFields  :: [FieldDef]
    , entityUniques :: [UniqueDef]
    , entityDerives :: [Text]
    , entityExtra   :: Map Text [ExtraLine]
    }
    deriving (Show, Eq, Read, Ord)

type ExtraLine = [Text]

newtype HaskellName = HaskellName { unHaskellName :: Text }
    deriving (Show, Eq, Read, Ord)
newtype DBName = DBName { unDBName :: Text }
    deriving (Show, Eq, Read, Ord)

type Attr = Text

-- | EmbedNone signifies a simple type (Text, Int64, etc) that can already be persisted.
-- An embeddded type is one that you also define in your persistent schema.
-- So we have a schema like:
--
-- @
--   [persistUpperCase|
--
--   Embedded no-migrate
--       name String
--
--   HasEmbeds
--       name   String
--       simple Embedded
--       list   [Embedded]
--       set    Set Embedded
-- @
--
-- Note the use of the no-migrate flag to tell SQL not to generate a migration for the embedded entity.
-- In our schema we demonstrate the different supported types:
--
--    * EmbedSimple: just embed a single entity
--    * EmbedList:   embed a list of entities
--    * EmbedSet:    embed a set of entities
data FieldType = EmbedNone   { unFieldType :: Text }
               | EmbedSimple { unFieldType :: Text }
               | EmbedList   { unFieldType :: Text }
               | EmbedSet    { unFieldType :: Text }
               deriving (Show, Eq, Read, Ord)

isEmbedded :: FieldDef -> Bool
isEmbedded fd = isEmbeddedType (fieldType fd)
  where
    isEmbeddedType (EmbedNone _) = False
    isEmbeddedType _ = True

data FieldDef = FieldDef
    { fieldHaskell :: HaskellName
    , fieldDB      :: DBName
    , fieldType    :: FieldType
    , fieldAttrs   :: [Attr]
    }
    deriving (Show, Eq, Read, Ord)

data UniqueDef = UniqueDef
    { uniqueHaskell :: HaskellName
    , uniqueDBName  :: DBName
    , uniqueFields  :: [(HaskellName, DBName)]
    }
    deriving (Show, Eq, Read, Ord)
