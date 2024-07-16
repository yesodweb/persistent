{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

-- | This module contains types and functions for working with and
-- disambiguating database and Haskell names.
--
-- @since 2.13.0.0
module Database.Persist.Names where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
-- Bring `Lift (Map k v)` instance into scope, as well as `Lift Text`
-- instance on pre-1.2.4 versions of `text`
import Instances.TH.Lift ()

-- | Convenience operations for working with '-NameDB' types.
--
-- @since 2.12.0.0
class DatabaseName a where
    escapeWith :: (Text -> str) -> (a -> str)

-- | A 'FieldNameDB' represents the datastore-side name that @persistent@
-- will use for a field.
--
-- @since 2.12.0.0
newtype FieldNameDB = FieldNameDB { unFieldNameDB :: Text }
    deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON FieldNameDB

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON FieldNameDB

-- | @since 2.12.0.0
instance DatabaseName FieldNameDB where
    escapeWith f (FieldNameDB n) = f n

-- | A 'FieldNameHS' represents the Haskell-side name that @persistent@
-- will use for a field.
--
-- @since 2.12.0.0
newtype FieldNameHS = FieldNameHS { unFieldNameHS :: Text }
    deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON FieldNameHS

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON FieldNameHS

-- | An 'EntityNameHS' represents the Haskell-side name that @persistent@
-- will use for an entity.
--
-- @since 2.12.0.0
newtype EntityNameHS = EntityNameHS { unEntityNameHS :: Text }
    deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON EntityNameHS

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON EntityNameHS

-- | An 'EntityNameDB' represents the datastore-side name that @persistent@
-- will use for an entity.
--
-- @since 2.12.0.0
newtype EntityNameDB = EntityNameDB { unEntityNameDB :: Text }
    deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON EntityNameDB

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON EntityNameDB

instance DatabaseName EntityNameDB where
    escapeWith f (EntityNameDB n) = f n

-- | A 'ConstraintNameDB' represents the datastore-side name that @persistent@
-- will use for a constraint.
--
-- @since 2.12.0.0
newtype ConstraintNameDB = ConstraintNameDB { unConstraintNameDB :: Text }
  deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON ConstraintNameDB

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON ConstraintNameDB

-- | @since 2.12.0.0
instance DatabaseName ConstraintNameDB where
  escapeWith f (ConstraintNameDB n) = f n

-- | An 'ConstraintNameHS' represents the Haskell-side name that @persistent@
-- will use for a constraint.
--
-- @since 2.12.0.0
newtype ConstraintNameHS = ConstraintNameHS { unConstraintNameHS :: Text }
  deriving (Show, Eq, Read, Ord, Lift, Generic)

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance FromJSON ConstraintNameHS

-- | Note: JSON representations not considered part of the public API; breaking
-- changes will not be reflected in the package version!
--
-- @since 2.14.7.0
instance ToJSON ConstraintNameHS
