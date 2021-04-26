-- |
--
-- @since 2.13.0.0
module Database.Persist.FieldDef
    ( -- * The 'FieldDef' type
      FieldDef
      -- ** Helpers
    , isFieldNotGenerated
      -- * 'FieldCascade'
    , FieldCascade(..)
    , renderFieldCascade
    , renderCascadeAction
    , noCascade
    , CascadeAction(..)
    ) where

import Database.Persist.FieldDef.Internal
