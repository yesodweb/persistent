module Database.Persist.EntityDef
    ( -- * Helper types
      HaskellName (..)
    , DBName (..)
    , Attr
      -- * Defs
    , EntityDef (..)
    , FieldDef (..)
    , FieldType (..)
    , UniqueDef (..)
    , ExtraLine
      -- * Utils
    , stripId
    ) where

import Data.Text (Text, stripSuffix, pack)
import Data.Map (Map)
import Database.Persist.Types

stripId :: FieldType -> Maybe Text
stripId (FTTypeCon Nothing t) = stripSuffix (pack "Id") t
stripId _ = Nothing
