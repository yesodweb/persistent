-- | This module exports many types and functions for operating on
-- @persistent@'s database representation. It's a bit of a kitchen sink. In the
-- future, this module will be reorganized, and many of the dependent modules
-- will be viewable on their own for easier documentation and organization.
module Database.Persist.Types
    (
    -- * Various Types of Names
    -- | There are so many kinds of names. @persistent@ defines newtype wrappers
    -- for 'Text' so you don't confuse what a name is and what it is
    -- supposed to be used for
      module Database.Persist.Names
    -- * Database Definitions
    -- ** Entity/Table Definitions
    -- | The 'EntityDef' type is used by @persistent@ to generate Haskell code,
    -- generate database migrations, and maintain metadata about entities. These
    -- are generated in the call to 'Database.Persist.TH.mkPersist'.
    , module Database.Persist.EntityDef
    -- ** Field definitions
    -- | The 'FieldDef' type is used to describe how a field should be
    -- represented at the Haskell and database layers.
    , module Database.Persist.FieldDef
    -- * Intermediate Values
    -- | The 'PersistValue' type is used as an intermediate layer between
    -- database and Haskell types.
    , module Database.Persist.PersistValue
    -- * Other Useful Stuff
    , SomePersistField (..)
    , Update (..)
    , BackendSpecificUpdate
    , SelectOpt (..)
    , Filter (..)
    , FilterValue (..)
    , BackendSpecificFilter
    , Key
    , Entity (..)
    , OverflowNatural(..)
    -- * The rest of the types
    , module Database.Persist.Types.Base
    ) where

import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistField
import Database.Persist.EntityDef
import Database.Persist.FieldDef
import Database.Persist.Names
import Database.Persist.PersistValue

-- this module is a bit of a kitchen sink of types and concepts. the guts of
-- persistent, just strewn across the table. in 2.13 let's get this cleaned up
-- and a bit more tidy.
import Database.Persist.Types.Base
       ( Attr
       , CascadeAction(..)
       , Checkmark(..)
       , CompositeDef(..)
       , EmbedEntityDef(..)
       , EmbedFieldDef(..)
       , ExtraLine
       , FieldAttr(..)
       , FieldCascade(..)
       , FieldDef(..)
       , FieldType(..)
       , ForeignDef(..)
       , ForeignFieldDef
       , IsNullable(..)
       , LiteralType(..)
       , PersistException(..)
       , PersistFilter(..)
       , PersistUpdate(..)
       , PersistValue(..)
       , ReferenceDef(..)
       , SqlType(..)
       , UniqueDef(..)
       , UpdateException(..)
       , WhyNullable(..)
       , fieldAttrsContainsNullable
       , keyAndEntityFields
       , noCascade
       , parseFieldAttrs
       )
