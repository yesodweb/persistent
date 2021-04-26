-- |
--
-- @since 2.13.0.0
module Database.Persist.FieldDef
    ( -- * The 'FieldDef' type
      FieldDef
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

import Database.Persist.Types.Base (FieldAttr(..))

-- | Returns 'True' if the 'FieldDef' does not have a 'MigrationOnly' or
-- 'SafeToRemove' flag from the QuasiQuoter.
--
-- @since 2.13.0.0
isHaskellField :: FieldDef -> Bool
isHaskellField fd =
    FieldAttrMigrationOnly `notElem` fieldAttrs fd &&
    FieldAttrSafeToRemove `notElem` fieldAttrs fd
