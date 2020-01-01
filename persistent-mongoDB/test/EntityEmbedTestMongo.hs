{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module EntityEmbedTestMongo where

-- because we are using a type alias we need to declare in a separate module
-- this is used in EmbedTest
import MongoInit

mkPersist persistSettings [persistUpperCase|
  ARecord
    name Text
    deriving Show Eq Read Ord
|]

type AnEntity = Entity ARecord
