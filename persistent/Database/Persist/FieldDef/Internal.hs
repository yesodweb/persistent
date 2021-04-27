-- | TODO: standard Internal moduel boilerplate
--
-- @since 2.13.0.0
module Database.Persist.FieldDef.Internal
    ( FieldDef(..)
    , isFieldNotGenerated
    , FieldCascade(..)
    , renderFieldCascade
    , renderCascadeAction
    , noCascade
    , CascadeAction(..)
    ) where

import Database.Persist.Types.Base
