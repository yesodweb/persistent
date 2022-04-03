{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Database.Persist.TH.NestedSymbolsInTypeSpecImports where

import Data.Proxy
import TemplateTestImports

data ReadOnly
data ReadWrite

newtype SomePath a = SomePath Text

instance PersistFieldSql (SomePath a) where
    sqlType _ = SqlString

instance PersistField (SomePath a) where
    toPersistValue (SomePath n) =
        toPersistValue n
    fromPersistValue v =
        SomePath <$> fromPersistValue v
