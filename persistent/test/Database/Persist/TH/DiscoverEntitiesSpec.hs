{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.DiscoverEntitiesSpec where

import TemplateTestImports

import Data.Aeson

import Data.Text (Text)

import Language.Haskell.TH.Syntax

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)

mkPersist sqlSettings [persistLowerCase|

User
    name    String
    age     Int

Dog
    user    UserId
    name    String

Cat
    enemy   DogId
    name    String

|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

$(pure [])

spec :: Spec
spec = describe "DiscoverEntitiesSpec" $ do
    let entities = $(discoverEntities)
    it "should have all three entities" $ do
        entities `shouldMatchList`
            [ entityDef $ Proxy @User
            , entityDef $ Proxy @Dog
            , entityDef $ Proxy @Cat
            ]
