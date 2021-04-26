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

module Database.Persist.TH.EmbedSpec where

import TemplateTestImports

import Data.Text (Text)

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)
import Database.Persist.Types

mkPersist sqlSettings [persistLowerCase|

Thing
    name String

    deriving Eq Show

EmbedThing
    someThing Thing

    deriving Eq Show

SelfEmbed
    name Text
    self SelfEmbed Maybe
    deriving Eq Show
|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = describe "EmbedSpec" $ do
    describe "EmbedThing" $ do
        it "generates" $ do
            let embedThing :: EmbedThing
                embedThing = EmbedThing (Thing "asdf")
            pass
