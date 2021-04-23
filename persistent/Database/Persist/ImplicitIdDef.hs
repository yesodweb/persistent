{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Persist.ImplicitIdDef
    ( -- * The Type
      ImplicitIdDef
      -- * Construction
    , mkImplicitIdDef
    -- * Autoincrementing Integer Key
    , autoIncrementingInteger
    -- * Getters
    -- * Setters
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

-- |
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
        }
