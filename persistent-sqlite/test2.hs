{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
-- some degenerate cases
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.Persist
import Database.Persist.Sqlite3
import Database.Persist.State
import Prelude hiding (filter)

derivePersistSqlite3 $
    Table
        "Foo"
        [ ("field", ("Int", False))
        ]
        []
        []
        []
        []

main = putStrLn "degenerates work!"
