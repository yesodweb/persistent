{-# LANGUAGE UndecidableInstances #-}
module EntityEmbedTest where

-- because we are using a type alias we need to declare in a separate module
-- this is used in EmbedTest
import Init

mkPersist persistSettings { mpsGeneric = True } [persistUpperCase|
  ARecord
    name Text
    deriving Show Eq Read Ord
|]

type AnEntity = Entity ARecord
