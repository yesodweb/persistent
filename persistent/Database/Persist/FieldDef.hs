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
    ( isHaskellField
    , FieldAttr
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
