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
module RecursiveMongo (specs) where

import InitMongo

mkPersist persistSettings [persistUpperCase|
SubType
  object [MenuObject]
  deriving Show Eq
MenuObject
  sub SubType Maybe
  deriving Show Eq
|]
cleanDB :: ReaderT Context IO ()
cleanDB = do
  deleteWhere ([] :: [Filter MenuObject])
  deleteWhere ([] :: [Filter SubType])
db :: Action IO () -> Assertion
db = db' cleanDB

specs :: Spec
specs = describe "recursive definitions" $ do
  it "mutually recursive" $ db $ do
    let m1 = MenuObject $ Just $ SubType []
    let m2 = MenuObject $ Just $ SubType [m1]
    let m3 = MenuObject $ Just $ SubType [m2]
    k3 <- insert m3
    m3' <- get k3
    m3' @== Just m3
