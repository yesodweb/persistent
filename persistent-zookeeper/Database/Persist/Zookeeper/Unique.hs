{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Zookeeper.Unique
       where

import Database.Persist
import qualified Database.Zookeeper as Z
import Database.Persist.Zookeeper.Internal
import Database.Persist.Zookeeper.Store()


instance PersistUniqueRead Z.Zookeeper where
    getBy uniqVal = do
      let key = uniqkey2key uniqVal
      val <- get key
      case val of
        Nothing -> return Nothing
        Just v -> return $ Just $ Entity key v

instance PersistUniqueWrite Z.Zookeeper where
    deleteBy uniqVal = do
      let key = uniqkey2key uniqVal
      delete key
