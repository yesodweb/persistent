-- some degenerate cases
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding (filter)
import Database.Persist
import Database.Persist.State
import Database.Persist.Sqlite3
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Database.HDBC.Sqlite3 (connectSqlite3)

derivePersistSqlite3 $ Table "Foo"
    [ ("field", ("Int", False))
    ]
    []
    []
    []
    []

main = putStrLn "degenerates work!"
