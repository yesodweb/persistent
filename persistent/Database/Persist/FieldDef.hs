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

setFieldAttrs :: [FieldAttr] -> FieldDef -> FieldDef
setFieldAttrs fas fd = fd { fieldAttrs = fas }

overFieldAttrs :: ([FieldAttr] -> [FieldAttr]) -> FieldDef -> FieldDef
overFieldAttrs k fd = fd { fieldAttrs = k (fieldAttrs fd) }

-- |
--
-- @since 2.13.0.0
addFieldAttr :: FieldAttr -> FieldDef -> FieldDef
addFieldAttr fa = overFieldAttrs (fa :)
