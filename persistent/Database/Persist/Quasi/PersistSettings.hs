module Database.Persist.Quasi.PersistSettings
    ( PersistSettings
    , defaultPersistSettings
    , upperCaseSettings
    , lowerCaseSettings
    , ParserErrorLevel (..)
    , ParserWarning
    , warningPos
    , parserWarningMessage

      -- ** Getters and Setters
    , getPsToDBName
    , setPsToDBName
    , setPsToFKName
    , setPsUseSnakeCaseForeignKeys
    , setPsUseSnakeCaseForiegnKeys
    , getPsStrictFields
    , setPsStrictFields
    , getPsIdName
    , setPsIdName
    , getPsTabErrorLevel
    , setPsTabErrorLevel
    , getPsQuotedArgumentErrorLevel
    , setPsQuotedArgumentErrorLevel
    ) where

import Database.Persist.Quasi.PersistSettings.Internal
