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
