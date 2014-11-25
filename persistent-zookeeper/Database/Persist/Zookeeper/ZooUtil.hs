{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Persist.Zookeeper.ZooUtil
    where

import qualified Database.Zookeeper as Z
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Monoid

deriving instance Read (Z.ZKError)
deriving instance Read (Z.Stat)

zGet :: Z.Zookeeper
     -> String
     -> String
     -> IO (Either Z.ZKError (Maybe B.ByteString, Z.Stat))
zGet zk dir key = do
  let path = dir <> "/" <> key
  Z.get zk path Nothing

zSet :: Z.Zookeeper
     -> String
     -> String
     -> Maybe B.ByteString
     -> Maybe Z.Version
     -> IO (Either Z.ZKError Z.Stat)
zSet zk dir key dat ver = do
  let path = dir <> "/" <> key
  Z.set zk path dat ver

zModify :: Z.Zookeeper
        -> String
        -> String
        -> (Maybe B.ByteString -> IO (Maybe B.ByteString))
        -> IO (Either Z.ZKError Z.Stat)
zModify  zk dir key f = do
  v <- zGet zk dir key
  case v of
    Right (con,ver) -> do
      v'' <- f con
      v' <- zSet zk dir key v'' (Just (Z.statVersion ver))
      case v' of
        Right _ -> return v'
        Left _ -> zModify zk dir key f
    Left e -> return $ Left e

zReplace :: Z.Zookeeper
         -> String
         -> String
         -> (Maybe B.ByteString)
         -> IO (Either Z.ZKError Z.Stat)
zReplace  zk dir key v'' = do
  v <- zGet zk dir key
  case v of
    Right (_con,ver) -> do
      v' <- zSet zk dir key v'' (Just (Z.statVersion ver))
      case v' of
        Right _ -> return v'
        Left _ -> zReplace zk dir key v''
    Left e -> return $ Left e

zRepSert :: Z.Zookeeper
       -> String
       -> String
       -> (Maybe B.ByteString)
       -> IO (Either Z.ZKError ())
zRepSert  zk dir key v'' = do
  v <- zCreate zk dir key v'' []
  case v of
    Right _ -> return $ Right ()
    Left Z.NodeExistsError -> do
      v' <- zReplace zk dir key v''
      case v' of
        Right _ -> return $ Right ()
        Left Z.NoNodeError -> do
          zRepSert zk dir key v''
        Left s -> do
          return $ Left s
    Left v' -> return $ Left v'


zGetChildren :: Z.Zookeeper
             -> String
             -> IO (Either Z.ZKError [String])
zGetChildren  zk dir = do
  v <- Z.getChildren zk dir Nothing
  case v of
    Right _ -> return v
    Left Z.NoNodeError -> return $ Right []
    Left _ -> return v

zCreate :: Z.Zookeeper
       -> String
       -> String
       -> Maybe B.ByteString
       -> [Z.CreateFlag]
       -> IO (Either Z.ZKError String)
zCreate zk dir key value flag = do
  let path = dir <> "/" <> key
  v <- Z.create zk path value Z.OpenAclUnsafe flag
  case v of
    Left Z.NoNodeError -> do
      v' <- Z.create zk dir Nothing Z.OpenAclUnsafe []
      case v' of
        Left _ -> return $ v'
        Right _ -> zCreate zk dir key value flag
    Left _ -> return v
--    See https://issues.apache.org/jira/browse/ZOOKEEPER-1027
--    Do not use libzookeeper under 3.3.5, Z.create returns wrong node path
--    Use libzookeeper over 3.4.*
    Right path' -> return $ Right $ drop (length ( dir <> "/" )) path'

zDelete :: Z.Zookeeper
        -> String
        -> String
        -> Maybe Z.Version
        -> IO (Either Z.ZKError ())
zDelete zk dir key mversion = do
  let path = dir <> "/" <> key
  Z.delete zk path mversion

zDeleteRecursive :: Z.Zookeeper
                 -> String
                 -> IO (Either Z.ZKError ())
zDeleteRecursive zk dir = do
  ls <- zGetTree zk dir
  res <- forM (reverse ls) $ \node ->
    Z.delete zk node Nothing
  return $ checkRes res
  where
    checkRes [] = Right ()
    checkRes (Left val:_) = Left val
    checkRes (Right _:xs) = checkRes xs

zGetTree :: Z.Zookeeper
         -> String
         -> IO [String]
zGetTree zk dir = do
  ls <- Z.getChildren zk dir Nothing
  case ls of
    Right dir' -> do
      ls' <- forM dir' $ \d -> do
        zGetTree zk (dir <> "/" <> d)
      return $ concat ls'
    Left err' -> error ("zGetTree's error:" ++ show err')
