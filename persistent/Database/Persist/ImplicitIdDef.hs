{-# LANGUAGE TemplateHaskellQuotes #-}

-- | This module contains types and functions for creating an 'ImplicitIdDef',
-- which allows you to customize the implied ID column that @persistent@
-- generates.
--
-- If this module doesn't suit your needs, you may want to import
-- "Database.Persist.ImplicitIdDef.Internal" instead. If you do so, please file
-- an issue on GitHub so we can support your needs. Breaking changes to that
-- module will *not* be accompanied with a major version bump.
--
-- @since 2.13.0.0
module Database.Persist.ImplicitIdDef
    ( -- * The Type
      ImplicitIdDef
      -- * Construction
    , mkImplicitIdDef
    -- * Autoincrementing Integer Key
    , autoIncrementingInteger
    -- * Getters
    -- * Setters
    , setImplicitIdDefMaxLen
    , unsafeClearDefaultImplicitId
    ) where

import Language.Haskell.TH

import Database.Persist.ImplicitIdDef.Internal
import Database.Persist.Types.Base
    ( FieldType(..)
    , SqlType(..)
    )
import Database.Persist.Class (BackendKey)
import Database.Persist.Names

-- | This is the default variant. Setting the implicit ID definition to this
-- value should not have any change at all on how entities are defined by
-- default.
--
-- @since 2.13.0.0
autoIncrementingInteger :: ImplicitIdDef
autoIncrementingInteger =
    ImplicitIdDef
        { iidFieldType = \entName ->
            FTTypeCon Nothing $ unEntityNameHS entName `mappend` "Id"
        , iidFieldSqlType =
            SqlInt64
        , iidType = \isMpsGeneric mpsBackendType ->
            ConT ''BackendKey `AppT`
                if isMpsGeneric
                then VarT (mkName "backend")
                else mpsBackendType
        , iidDefault =
            Nothing
        , iidMaxLen =
            Nothing
        }
