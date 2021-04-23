module Database.Persist.SqlBackend.Internal.InsertSqlResult where

import Database.Persist.Types.Base (PersistValue)
import Data.Text (Text)

data InsertSqlResult
    = ISRSingle Text
    | ISRInsertGet Text Text
    | ISRManyKeys Text [PersistValue]
