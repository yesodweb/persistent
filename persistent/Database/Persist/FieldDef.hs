-- |
--
-- @since 2.13.0.0
module Database.Persist.FieldDef
    ( -- * The 'FieldDef' type
      FieldDef
      -- ** Setters
    , setFieldAttrs
    , overFieldAttrs
    , addFieldAttr
      -- ** Helpers
    , isFieldNullable
    , isFieldMaybe
    , isFieldNotGenerated
    , isHaskellField
      -- * 'FieldCascade'
    , FieldCascade(..)
    , renderFieldCascade
    , renderCascadeAction
    , noCascade
    , CascadeAction(..)
    ) where

import Database.Persist.FieldDef.Internal

import Database.Persist.Types.Base
       ( FieldAttr(..)
       , FieldType(..)
       , IsNullable(..)
       , fieldAttrsContainsNullable
       , isHaskellField
       )

-- | Replace the 'FieldDef' 'FieldAttr' with the new list.
--
-- @since 2.13.0.0
setFieldAttrs :: [FieldAttr] -> FieldDef -> FieldDef
setFieldAttrs fas fd = fd { fieldAttrs = fas }

-- | Modify the list of field attributes.
--
-- @since 2.13.0.0
overFieldAttrs :: ([FieldAttr] -> [FieldAttr]) -> FieldDef -> FieldDef
overFieldAttrs k fd = fd { fieldAttrs = k (fieldAttrs fd) }

-- | Add an attribute to the list of field attributes.
--
-- @since 2.13.0.0
addFieldAttr :: FieldAttr -> FieldDef -> FieldDef
addFieldAttr fa = overFieldAttrs (fa :)

-- | Check if the field definition is nullable
--
-- @since 2.13.0.0
isFieldNullable :: FieldDef -> IsNullable
isFieldNullable =
    fieldAttrsContainsNullable . fieldAttrs

-- | Check if the field is `Maybe a`
--
-- @since 2.13.0.0
isFieldMaybe :: FieldDef -> Bool
isFieldMaybe field =
    case fieldType field of
        FTApp (FTTypeCon _ "Maybe") _ ->
            True
        _ ->
            False
