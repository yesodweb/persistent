{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.RDBMS.Utils where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql (rawExecute, SqlBackend)
import Data.Monoid ((<>))

deleteAllRows
  :: (MonadIO m, PersistEntity record)
  => record -> ReaderT SqlBackend m ()
deleteAllRows entity = rawExecute ("delete from " <> tbName) []
  where
    tbName = unDBName $ entityDB $ entityDef ent
    ent = Just entity
