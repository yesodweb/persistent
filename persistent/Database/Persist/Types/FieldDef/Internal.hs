-- |  This module contains internal definitions for the 'FieldDef' type.
-- Breaking changes to the interface of this module will not be represented
-- as a breaking change in the version of the code. Please depend on
-- "Database.Persist.Types.FieldDef" instead. If you need this module,
-- please file an issue on GitHub.
--
-- @since 2.13.0.0
module Database.Persist.Types.FieldDef.Internal where

import Database.Persist.Types.Names
import Language.Haskell.TH.Syntax (Lift)
import Data.Text (Text)

-- | A 'FieldDef' represents the inormation that @persistent@ knows about
-- a field of a datatype. This includes information used to parse the field
-- out of the database and what the field corresponds to.
data FieldDef = FieldDef
    { fieldHaskell   :: !FieldNameHS
    -- ^ The name of the field. Note that this does not corresponds to the
    -- record labels generated for the particular entity - record labels
    -- are generated with the type name prefixed to the field, so
    -- a 'FieldDef' that contains a @'FieldNameHS' "name"@ for a type
    -- @User@ will have a record field @userName@.
    , fieldDB        :: !FieldNameDB
    -- ^ The name of the field in the database. For SQL databases, this
    -- corresponds to the column name.
    , fieldType      :: !FieldType
    -- ^ The type of the field in Haskell.
    , fieldSqlType   :: !SqlType
    -- ^ The type of the field in a SQL database.
    , fieldAttrs     :: ![FieldAttr]
    -- ^ User annotations for a field. These are provided with the @!@
    -- operator.
    , fieldStrict    :: !Bool
    -- ^ If this is 'True', then the Haskell datatype will have a strict
    -- record field. The default value for this is 'True'.
    , fieldReference :: !ReferenceDef
    , fieldCascade :: !FieldCascade
    -- ^ Defines how operations on the field cascade on to the referenced
    -- tables. This doesn't have any meaning if the 'fieldReference' is set
    -- to 'NoReference' or 'SelfReference'. The cascade option here should
    -- be the same as the one obtained in the 'fieldReference'.
    --
    -- @since 2.11.0
    , fieldComments  :: !(Maybe Text)
    -- ^ Optional comments for a 'Field'. There is not currently a way to
    -- attach comments to a field in the quasiquoter.
    --
    -- @since 2.10.0
    , fieldGenerated :: !(Maybe Text)
    -- ^ Whether or not the field is a @GENERATED@ column, and additionally
    -- the expression to use for generation.
    --
    -- @since 2.11.0.0
    }
    deriving (Show, Eq, Read, Ord, Lift)
