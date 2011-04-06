{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Persist.Quasi
import Database.Persist.TH
import Prelude hiding (filter)
import Database.Persist
import Database.Persist.MongoDB
import qualified Network.Abstract(NetworkIO)
import Control.Monad.Context
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Database.Persist.GenericSql
import qualified Database.MongoDB as DB
import Control.Monad.Throw
import Test.HUnit

mkPersist [persist|
Person 
    name String Update Eq Ne Desc In
    age Int Update "Asc" Lt "some ignored attribute"
    color String Maybe Eq Ne NotIn Ge
    PersonNameKey name
Pet
    owner PersonId
    name String
Null
    field Int Maybe Eq Ne Gt NotIn In
Table
    table String
|]


