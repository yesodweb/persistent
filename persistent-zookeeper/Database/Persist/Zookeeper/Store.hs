{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Zookeeper.Store (
  deleteRecursive
, BackendKey(..)
)where

import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Zookeeper as Z
import Data.Monoid
import qualified Data.Text as T
import Database.Persist.Zookeeper.Config
import Database.Persist.Zookeeper.Internal
import Database.Persist.Zookeeper.ZooUtil
import Control.Monad
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Web.PathPieces (PathPiece (..))

-- | ToPathPiece is used to convert a key to/from text
instance PathPiece (BackendKey Z.Zookeeper) where
    toPathPiece key = "z" <> (unZooKey key)
    fromPathPiece keyText =
      case T.uncons keyText of
        Just ('z', prefixed) -> Just $ ZooKey prefixed
        _ -> mzero

instance Sql.PersistFieldSql (BackendKey Z.Zookeeper) where
    sqlType _ = Sql.SqlOther "doesn't make much sense for Zookeeper"

instance A.ToJSON (BackendKey Z.Zookeeper) where
    toJSON (ZooKey key) = A.toJSON $ "z" <> key

instance A.FromJSON (BackendKey Z.Zookeeper) where
    parseJSON v = A.modifyFailure ("Persistent: error loading Zookeeper conf: " ++) $
      flip (A.withText "ZooKey") v $ \t ->
        case T.uncons t of
          Just ('z', prefixed) -> return $ ZooKey prefixed
          _ -> (fail "Invalid json for zookey")


deleteRecursive :: (Monad m, MonadIO m) => String -> Action m ()
deleteRecursive dir = execZookeeper $ \zk -> zDeleteRecursive zk dir


instance PersistStore Z.Zookeeper where
    newtype BackendKey Z.Zookeeper = ZooKey { unZooKey :: T.Text }
        deriving (Show, Read, Eq, Ord, PersistField)

    insert val = do
      mUniqVal <- val2uniqkey val
      case mUniqVal of
        Just uniqVal -> do
          let key = (uniqkey2key uniqVal)
          execZookeeper $ \zk -> do
            let dir = entity2path val
            r <- zCreate zk dir (keyToTxt key) (Just (entity2bin val)) []
            case r of
              Right _ -> return $ Right $ key
--              Left Z.NodeExistsError -> return $ Right $ Nothing
              Left v -> return $ Left v
        Nothing -> do
          let dir = entity2path val
          str <- execZookeeper $ \zk -> do
            zCreate zk dir "" (Just (entity2bin val)) [Z.Sequence]
          return $ txtToKey str

    insertKey key val = do
      _ <- execZookeeper $ \zk -> do
        let dir = entity2path val
        zCreate zk dir (keyToTxt key) (Just (entity2bin val)) []
      return ()

    repsert key val = do
      _ <- execZookeeper $ \zk -> do
        let dir = entity2path val
        zRepSert zk dir (keyToTxt key) (Just (entity2bin val))
      return ()

    replace key val = do
      execZookeeper $ \zk -> do
        let dir = entity2path val
        _ <- zReplace zk dir (keyToTxt key) (Just (entity2bin val))
        return $ Right ()
      return ()

    delete key = do
      execZookeeper $ \zk -> do
        let dir = key2path key
        _ <- zDelete zk dir (keyToTxt key) Nothing
        return $ Right ()
      return ()

    get key = do
      r <- execZookeeper $ \zk -> do
        let dir = key2path key
        val <- zGet zk dir (keyToTxt key)
        return $ Right val
      case r of
        (Left Z.NoNodeError) ->
          return Nothing
        (Left v) ->
          fail $ show v
        (Right (Just str,_sta)) -> do
          return (bin2entity str)
        (Right (Nothing,_stat)) -> do
          fail $ "data is nothing"

    update key valList = do
      va <- get key
      case va of
        Nothing -> return ()
        Just v ->
          case updateEntity v valList of
            Right v' ->
              replace key v'
            Left v' -> error $ show v'
