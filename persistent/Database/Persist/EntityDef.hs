module Database.Persist.EntityDef
    ( -- * Helper types
      HaskellName (..)
    , DBName (..)
    , FieldType (..)
    , Attr
      -- * Defs
    , EntityDef (..)
    , FieldDef (..)
    , UniqueDef (..)
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
    , entityExtra   :: Map Text [Text]
    }
    deriving (Show, Eq, Read, Ord)

newtype HaskellName = HaskellName { unHaskellName :: Text }
    deriving (Show, Eq, Read, Ord)
newtype DBName = DBName { unDBName :: Text }
    deriving (Show, Eq, Read, Ord)
newtype FieldType = FieldType { unFieldType :: Text }
    deriving (Show, Eq, Read, Ord)

type Attr = Text

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
