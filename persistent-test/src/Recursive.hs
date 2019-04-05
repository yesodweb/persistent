{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Recursive (specs, specsWith
#ifndef WITH_NOSQL
 , recursiveMigrate
#endif
 , cleanup
) where

import Init

#if WITH_NOSQL
mkPersist persistSettings { mpsGeneric = True }[persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True }, mkMigrate "recursiveMigrate"] [persistLowerCase|
#endif
SubType
  object [MenuObject]
  deriving Show Eq
MenuObject
  sub SubType Maybe
  deriving Show Eq
|]

cleanup
    :: (PersistStoreWrite (BaseBackend backend), PersistQueryWrite backend)
    => ReaderT backend IO ()
cleanup = do
  deleteWhere ([] :: [Filter (MenuObjectGeneric backend)])
  deleteWhere ([] :: [Filter (SubTypeGeneric backend)])

#if WITH_NOSQL
cleanDB :: ReaderT Context IO ()
cleanDB = cleanup
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "recursive definitions" $ do
  it "mutually recursive" $ db $ do
    let m1 = MenuObject $ Just $ SubType []
    let m2 = MenuObject $ Just $ SubType [m1]
    let m3 = MenuObject $ Just $ SubType [m2]
    k3 <- insert m3
    m3' <- get k3
    m3' @== Just m3

specsWith
    ::
    ( PersistStoreWrite backend
    , PersistStoreWrite (BaseBackend backend)
    , MonadIO m
    )
    => RunDb backend m
    -> Spec
specsWith runDb = describe "recursive definitions" $ do
  it "mutually recursive" $ runDb $ do
    let m1 = MenuObject $ Just $ SubType []
    let m2 = MenuObject $ Just $ SubType [m1]
    let m3 = MenuObject $ Just $ SubType [m2]
    k3 <- insert m3
    m3' <- get k3
    m3' @== Just m3
